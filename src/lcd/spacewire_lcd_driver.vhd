-- Filename:     example_driver.vhd
-- Filetype:     VHDL Source Code
-- Date:         26 oct 2012
-- Update:       -
-- Description:  VHDL Description of example driver
-- Author:       J. op den Brouw
-- State:        Demo
-- Error:        -
-- Version:      1.1alpha
-- Copyright:    (c)2012, De Haagse Hogeschool

-- This VHDL code is a example description on how to use the
-- HD44780 LCD display driver module. It writes 4x20 characters
-- to the display, presuming that the display has four lines.
--
-- This code is tested on a Terasic DE0-board with an optional
-- LCD display. See the weblinks
-- http://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&CategoryNo=56&No=364
-- http://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&CategoryNo=78&No=396
-- for more info. The display used has only two lines.

-- After a line has written completely, the cursor is moved to
-- the beginning of the next line. After the last line is written,
-- this code goes into hold mode.
--

-- Libraries et al.
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE ieee.math_real.ALL;

-- The entity of a Terasic DE0-board.
ENTITY spacewire_lcd_driver IS
	PORT (
		CLOCK_50 : IN STD_LOGIC; -- DE0 CLOCK_50 (50MHz CLK)
		KEY : IN STD_LOGIC_VECTOR(1 DOWNTO 0); -- DE0 KEY (button) [reset]
		LED : OUT STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0'); -- DE0 LEDs [LED0 is 1 when writing a char]
		-- External LCD ports
		LCD_EN : OUT STD_LOGIC;
		LCD_RS : OUT STD_LOGIC;
		LCD_RW : OUT STD_LOGIC;
		LCD_DATA : INOUT STD_LOGIC_VECTOR(7 DOWNTO 0)
	);

END ENTITY spacewire_lcd_driver;

-- The architecture!
ARCHITECTURE hardware OF spacewire_lcd_driver IS

	-- Adds empty characters at the end of string, to 
	FUNCTION stringPadding(str : STRING; length : NATURAL) RETURN STRING IS
		VARIABLE paddedString : STRING(1 TO length) := (OTHERS => ' ');
	BEGIN
		paddedString(1 TO str'length) := str;
		RETURN paddedString;
	END FUNCTION;

	-- Data converting function

	-- accel_data is 16 bit vector (2s Complement) 
	-- function converts it to a REAL value
	-- than converts it to a string to the given decimal point precision
	-- Returns a formatted string with a prefix and suffix.
	-- Make sure your string won't exceed 20 characters (16 due to bug with LCD display)

	-- "selected range" is the value range chosen on the accelerometer, default is +/- 2g, for correct interpretation of data
	FUNCTION convertData(accel_data : STD_LOGIC_VECTOR(15 DOWNTO 0);
		prefix : STRING;
		suffix : STRING;
		selected_range : INTEGER;
		precision : NATURAL) RETURN STRING IS
		VARIABLE accel_value : REAL;
		VARIABLE accel_value_str : STRING(1 TO 20);
		VARIABLE temp_string : STRING(1 TO 20); -- Adjust the length based on your needs
	BEGIN
		-- Convert to REAL, scaled for correct interpretation (check accelerometer datasheet)
		accel_value := REAL(TO_INTEGER(SIGNED(accel_data))) * (REAL(selected_range) / 2.0 ** 12.0);

		-- Convert REAL to string, add padding to 20 to avoid length errors
		accel_value_str := stringPadding(real'image(accel_value), 20);

		-- Return formatted string, with correct decimal point precision
		RETURN stringPadding(prefix & accel_value_str(1 TO (precision + 2)) & suffix, 20);

	END FUNCTION;

	-- Component declaration of the LCD module
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
		update_linecount_wait, write_char_1, write_char_1_wait,
		write_char_2, write_char_2_wait, write_char_3, write_char_4, hold);
	SIGNAL state : state_type;

	-- A string of 20 characters
	SUBTYPE string20_type IS STRING(1 TO 20);
	-- An array of 4 strings of 20 characters.
	TYPE message4x20_type IS ARRAY (1 TO 4) OF string20_type;

	-- The four-line message
	CONSTANT message : message4x20_type :=
	(1 => stringPadding("Spacewire", 20),
	2 => convertData("1101101111011011", "x: ", "g", 2, 4),
	3 => convertData("0011100100101010", "    y: ", "g", 2, 4),
	4 => convertData("1111110000101110", "    z: ", "g", 2, 4));

	-- Counts the characters on a line.
	SIGNAL character_counter : INTEGER RANGE 1 TO 20;
	-- Counts the lines.
	SIGNAL line_counter : INTEGER RANGE 1 TO 4;

BEGIN

	-- Push buttons are active low.
	areset <= NOT KEY(0);

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

	-- The client side
	drive : PROCESS (clk, areset) IS
		VARIABLE aline : string20_type;
	BEGIN
		IF areset = '1' THEN
			wr <= '0';
			init <= '0';
			cls <= '0';
			home <= '0';
			goto10 <= '0';
			goto20 <= '0';
			goto30 <= '0';
			LED(0) <= '0';
			data <= "00000000";
			character_counter <= 1;
			state <= reset;
		ELSIF rising_edge(clk) THEN
			wr <= '0';
			init <= '0';
			cls <= '0';
			home <= '0';
			goto10 <= '0';
			goto20 <= '0';
			goto30 <= '0';
			LED(0) <= '0';
			data <= "00000000";
			CASE state IS

				WHEN reset =>
					-- Wait for the LCD module ready
					IF busy = '0' THEN
						state <= write_char;
					END IF;
					-- Setup message counter, start at 1.
					character_counter <= 1;
					line_counter <= 1;

				WHEN write_char =>
					LED(0) <= '1';
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
					LED(0) <= '1';
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

					-- The "hohouwer"
				WHEN hold =>
					state <= hold;

				WHEN OTHERS =>
					NULL;

			END CASE;
		END IF;
	END PROCESS;
END ARCHITECTURE hardware;