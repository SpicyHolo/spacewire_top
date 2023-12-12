-- Design based on : 
--  Filename:     example_driver.vhd
--  Filetype:     VHDL Source Code
--  Date:         26 oct 2012
--  Update:       -
--  Description:  VHDL Description of example driver
--  Author:       J. op den Brouw
--  State:        Demo
--  Error:        -
--  Version:      1.1alpha
--  Copyright:    (c)2012, De Haagse Hogeschool

-- This VHDL code is a example description on how to use the
-- HD44780 LCD display driver module. It writes 4x20 characters
-- to the display, presuming that the display has four lines.

-- Libraries et al.
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

-- The entity of a Terasic DE0-board.
ENTITY spacewire_lcd_driver IS
	PORT (
		CLOCK_50 : IN STD_LOGIC; -- DE0 CLOCK_50
		KEY : IN STD_LOGIC; -- DE0 KEY (button) [reset]
		KEY2 : IN STD_LOGIC; -- DE0 KEY2 (button) [set]
		LED : OUT STD_LOGIC_VECTOR(7 DOWNTO 0); -- DE0 LEDs

		-- External LCD ports
		LCD_EN : OUT STD_LOGIC;
		LCD_RS : OUT STD_LOGIC;
		LCD_RW : OUT STD_LOGIC;
		LCD_DATA : INOUT STD_LOGIC_VECTOR(7 DOWNTO 0);

		-- Data in
		data_in : IN STD_LOGIC_VECTOR(3 DOWNTO 0)
	);

END ENTITY spacewire_lcd_driver;

ARCHITECTURE hardware OF spacewire_lcd_driver IS

	-- Adds empty characters at the end of string, to change it's length
	FUNCTION stringPadding(str : STRING; length : NATURAL) RETURN STRING IS
		VARIABLE paddedString : STRING(1 TO length) := (OTHERS => ' ');
	BEGIN
		paddedString(1 TO str'length) := str;
		RETURN paddedString;
	END FUNCTION;

	-- Data converting function
	-- Converts 4-bit vector (2s complement code) into a string
	FUNCTION convertData(data_in : STD_LOGIC_VECTOR(3 DOWNTO 0)) RETURN STRING IS
		VARIABLE data_in_int : INTEGER;
		VARIABLE return_str : STRING(1 TO 20);
	BEGIN
		-- Convert 4-bit vector to an Integer
		data_in_int := TO_INTEGER(SIGNED(data_in));

		-- Adjust string to LCD line length
		return_str := stringPadding(INTEGER'image(data_in_int), 20);
		RETURN return_str;
	END FUNCTION;

	-- LCD Module
	COMPONENT lcd_driver_hd44780_module IS
		GENERIC (
			freq : INTEGER := 50000000;
			areset_pol : STD_LOGIC := '1';
			time_init1 : TIME := 40 ms;
			time_init2 : TIME := 4100 us;
			time_init3 : TIME := 100 us;
			time_tas : TIME := 60 ns;
			time_cycle_e : TIME := 1000 ns;
			time_pweh : TIME := 500 ns;
			time_no_bf : TIME := 2 ms;
			cursor_on : BOOLEAN := false;
			blink_on : BOOLEAN := false;
			use_bf : BOOLEAN := true
		);
		PORT (
			clk : IN STD_LOGIC;
			areset : IN STD_LOGIC;
			-- User site
			init : IN STD_LOGIC;
			data : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
			wr : IN STD_LOGIC;
			cls : IN STD_LOGIC;
			home : IN STD_LOGIC;
			goto10 : IN STD_LOGIC;
			goto20 : IN STD_LOGIC;
			goto30 : IN STD_LOGIC;
			busy : OUT STD_LOGIC;
			-- LCD side
			LCD_E : OUT STD_LOGIC;
			LCD_RS : OUT STD_LOGIC;
			LCD_RW : OUT STD_LOGIC;
			LCD_DB : INOUT STD_LOGIC_VECTOR(7 DOWNTO 0)
		);
	END COMPONENT lcd_driver_hd44780_module;

	-- The system's frequency
	CONSTANT sys_freq : INTEGER := 50000000;

	SIGNAL areset : STD_LOGIC;
	SIGNAL set : STD_LOGIC;

	SIGNAL clk : STD_LOGIC;
	SIGNAL init : STD_LOGIC;
	SIGNAL data : STD_LOGIC_VECTOR(7 DOWNTO 0);
	SIGNAL wr : STD_LOGIC;
	SIGNAL cls : STD_LOGIC;
	SIGNAL home : STD_LOGIC;
	SIGNAL goto10 : STD_LOGIC;
	SIGNAL goto20 : STD_LOGIC;
	SIGNAL goto30 : STD_LOGIC;
	SIGNAL busy : STD_LOGIC;

	TYPE state_type IS (reset, write_char, write_char_wait, update, update_linecount,
		update_linecount_wait, hold, update_home, update_home_wait);
	SIGNAL state : state_type;

	-- A string of 20 characters
	SUBTYPE string20_type IS STRING(1 TO 20);

	-- An array of 4 strings of 20 characters.
	TYPE message4x20_type IS ARRAY (1 TO 4) OF string20_type;

	SIGNAL message : message4x20_type;

	-- Counts the characters on a line.
	SIGNAL character_counter : INTEGER RANGE 1 TO 20;

	-- Counts the lines.
	SIGNAL line_counter : INTEGER RANGE 1 TO 4;

BEGIN
	-- Push buttons are active low.
	areset <= NOT KEY;
	set <= NOT KEY2;

	-- The clock
	clk <= CLOCK_50;

	-- Use LCD module.
	lcdm : lcd_driver_hd44780_module
	GENERIC MAP(
		freq => sys_freq, areset_pol => '1', time_cycle_e => 2000 ns, time_pweh => 500 ns,
		cursor_on => false, blink_on => false, use_bf => false)
	PORT MAP(
		clk => clk, areset => areset, init => init, data => data, wr => wr, cls => cls,
		home => home, goto10 => goto10, goto20 => goto20, goto30 => goto30, busy => busy,
		LCD_E => LCD_EN, LCD_RS => LCD_RS, LCD_RW => LCD_RW, LCD_DB => LCD_DATA);

	drive : PROCESS (clk, areset, data_in) IS
		VARIABLE aline : string20_type;
		VARIABLE temp : string20_type;

	BEGIN
		IF areset = '1' THEN
			LED <= (OTHERS => '0');
			-- Initialise LCD lines
			message(1) <= "Initial             ";
			message(2) <= "                    ";
			message(3) <= "                    ";
			message(4) <= "                    ";

			wr <= '0';
			init <= '0';
			cls <= '0';
			home <= '0';
			goto10 <= '0';
			goto20 <= '0';
			goto30 <= '0';
			data <= "00000000";
			character_counter <= 1;
			state <= reset;
		ELSIF rising_edge(clk) THEN

			-- Convert input data, set the second LCD message to its value (on button press)
			-- If not, go back to Initial text
			IF set = '1' THEN
				temp := convertData(data_in);
				LED(3 DOWNTO 0) <= data_in; -- Debug LEDs
				message(1) <= temp;
			ELSE
				LED(3 DOWNTO 0) <= (OTHERS => '0'); -- Debug LEDs
				message(1) <= "Initial             "; -- Set LCD 1st line to INITIAL
			END IF;
			wr <= '0';
			init <= '0';
			cls <= '0';
			home <= '0';
			goto10 <= '0';
			goto20 <= '0';
			goto30 <= '0';
			data <= "00000000";
			CASE state IS

				WHEN reset => -- Initial state
					-- Wait for the LCD module ready
					IF busy = '0' THEN
						state <= write_char;
					END IF;

					-- Setup message counter, start at 1.
					character_counter <= 1;
					line_counter <= 1;

				WHEN write_char =>
					-- Set up WRITE!
					-- Use the data from the string
					aline := message(line_counter);
					data <= STD_LOGIC_VECTOR(to_unsigned(CHARACTER'pos(aline(character_counter)), 8));
					wr <= '1';
					state <= write_char_wait;

				WHEN write_char_wait =>
					-- This state is needed so that the LCD driver
					-- can process the write command. Note that data
					-- and wr are registered outputs and get their
					-- respective values while in *this* state. If you don't
					-- want this behaviour, please make your outputs
					-- non-registered.
					state <= update;

				WHEN update =>
					-- Wait for the write complete
					IF busy = '0' THEN
						-- If end of string, goto hold mode...
						IF line_counter = 4 AND character_counter = 20 THEN
							state <= hold;
							-- If end of line...    
						ELSIF character_counter = 20 THEN
							CASE line_counter IS
								WHEN 1 => goto10 <= '1';
								WHEN 2 => goto20 <= '1';
								WHEN 3 => goto30 <= '1';
									-- Never reached, but nice anyway...
								WHEN 4 => home <= '1';
								WHEN OTHERS => NULL;
							END CASE;
							-- Set new values of the counters
							line_counter <= line_counter + 1;
							character_counter <= 1;
							-- Goto the update state
							state <= update_linecount;
						ELSE
							-- Not the end of a lines, update the character counter.
							character_counter <= character_counter + 1;
							state <= write_char;
						END IF;
					END IF;
				WHEN update_linecount =>
					-- This state is needed so that the LCD driver
					-- can process the gotoXX command. Note that the gotoXX
					-- signals are registered outputs and get their
					-- respective values while in *this* state. If you don't
					-- want this behaviour, please make your outputs
					-- non-registered.
					state <= update_linecount_wait;

				WHEN update_linecount_wait =>
					-- Wait for the LCD module ready
					IF busy = '0' THEN
						state <= write_char;
					END IF;

					-- Finished writing, go to home (line 0, char 0)
				WHEN hold =>
					home <= '1';
					state <= update_home;

					-- Analogous to update_linecount
				WHEN update_home =>
					state <= update_home_wait;

					-- Analogous to update_linecount_wait
				WHEN update_home_wait =>
					IF busy = '0' THEN
						state <= reset;
					END IF;
			END CASE;
		END IF;
	END PROCESS;
END ARCHITECTURE hardware;