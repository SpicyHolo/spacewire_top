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
		data_x : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
		data_y : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
		data_z : IN STD_LOGIC_VECTOR(15 DOWNTO 0)
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
 
	TYPE ROW_DATA IS ARRAY(1 to 20) OF STD_LOGIC_VECTOR(7 downto 0);
	CONSTANT title_row : ROW_DATA := (
        1 => X"53", 2 => X"70", 3 => X"61", 4 => X"63",
        5 => X"65", 6 => X"57", 7 => X"69", 8 => X"72",
        9 => X"65", 10 => X"20", 11 => X"41", 12 => X"63",
        13 => X"63", 14 => X"65", 15 => X"6C", 16 => X"20",
        17 => X"20", 18 => X"20", 19 => X"20", 20 => X"20"
	);

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

	SIGNAL data_x_abs: STD_LOGIC_VECTOR(15 downto 0);
	SIGNAL data_y_abs: STD_LOGIC_VECTOR(15 downto 0);
	SIGNAL data_z_abs: STD_LOGIC_VECTOR(15 downto 0);
	SIGNAL data_x_sign: STD_LOGIC;
	SIGNAL data_y_sign: STD_LOGIC;
	SIGNAL data_z_sign: STD_LOGIC;

	SIGNAL bcd_display_data : STD_LOGIC_VECTOR(19 downto 0);
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

	drive : PROCESS (clk, areset) IS
		VARIABLE aline : string20_type;
		VARIABLE temp : string20_type;
		VARIABLE temp_data_vec : STD_LOGIC_VECTOR(15 downto 0);
		VARIABLE bcd : STD_LOGIC_VECTOR(19 downto 0);
		VARIABLE char : STD_LOGIC_VECTOR(7 DOWNTO 0);
		VARIABLE bcd_block : INTEGER := 0;
	BEGIN


		IF rising_edge(clk) THEN
			IF areset = '1' THEN
				-- Initialise LCD lines
				message(1) <= "RESET               ";
				message(2) <= "                    ";
				message(3) <= "                    ";
				message(4) <= "                    ";
				state <= reset;
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
						
						IF data_x(9) = '0' THEN
							data_x_sign <= '0';
							temp_data_vec := data_x;
							data_x_abs <= temp_data_vec;
						ELSE
							data_x_sign <= '1';
							temp_data_vec := NOT data_x;
							temp_data_vec := STD_LOGIC_VECTOR(unsigned(temp_data_vec) + 1);
							data_x_abs <= temp_data_vec;
						END IF;

						bcd := (others => '0');		 	
						for i in 0 to 15 loop					--Iterate once for each bit in input number
							if unsigned(bcd(3 downto 0)) >= 5 then
								bcd(3 downto 0) := STD_LOGIC_VECTOR(unsigned(bcd(3 downto 0)) + 3);	
							end if;	--If any BCD digit is >= 5, add three
							if unsigned(bcd(7 downto 4)) >= 5 then
								bcd(7 downto 4) := STD_LOGIC_VECTOR(unsigned(bcd(7 downto 4)) + 3);	
							end if;
							if unsigned(bcd(11 downto 8)) >= 5 then
								bcd(11 downto 8) := STD_LOGIC_VECTOR(unsigned(bcd(11 downto 8)) + 3);	
							end if;
							if unsigned(bcd(15 downto 12)) >= 5 then
								bcd(15 downto 12) := STD_LOGIC_VECTOR(unsigned(bcd(15 downto 12)) + 3);	
							end if;
							if unsigned(bcd(19 downto 16)) >= 5 then
								bcd(19 downto 16) := STD_LOGIC_VECTOR(unsigned(bcd(19 downto 16)) + 3);	
							end if;

							bcd := bcd(18 downto 0) & temp_data_vec(15-i);				--Shift one bit, and shift in proper bit from input 
						end loop;
						bcd_display_data <= bcd;
						state <= write_char;
						-- Setup message counter, start at 1.
						character_counter <= 1;
						line_counter <= 1;
					END IF;

				WHEN write_char =>
					CASE line_counter IS
						WHEN 1 => char := title_row(character_counter);
						WHEN 2 => 
							IF character_counter = 1 THEN
								CASE data_x_sign IS
									WHEN '0' =>
										char := X"2b";
									WHEN '1' =>
										char := X"2d";
								END CASE;
							ELSIF character_counter > 1 AND character_counter <= 17 THEN
								CASE data_x_abs(17 - character_counter) IS
									WHEN '0' =>
										char := X"30";
									WHEN '1' =>
										char := X"31";
								END CASE;
							ELSE
								char := X"20";
							END IF;
						WHEN 3 => 
							IF character_counter = 1 THEN
								CASE data_x_sign IS
									WHEN '0' =>
										char := X"2b";
									WHEN '1' =>
										char := X"2d";
								END CASE;
							ELSIF character_counter > 1 AND character_counter <= 6 THEN
								bcd_block := (6 - character_counter) * 4;
								CASE bcd_display_data((bcd_block + 3) downto bcd_block) IS
									WHEN "0000" =>
										char := X"30";
									WHEN "0001" =>
										char := X"31";
									WHEN "0010" =>
										char := X"32";
									WHEN "0011" =>
										char := X"33";
									WHEN "0100" =>
										char := X"34";
									WHEN "0101" =>
										char := X"35";
									WHEN "0110" =>
										char := X"36";
									WHEN "0111" =>
										char := X"37";
									WHEN "1000" =>
										char := X"38";
									WHEN "1001" =>
										char := X"39";
									WHEN OTHERS =>
										char := X"20";
								END CASE;
							ELSE
								char := X"20";
							END IF;
						WHEN 4 => 
							IF character_counter <= 16 THEN
								CASE data_z(16 - character_counter) IS
									WHEN '0' =>
										char := X"30";
									WHEN '1' =>
										char := X"31";
								END CASE;
							ELSE
								char := X"20";
							END IF;							
					END CASE;
					data <= char;
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
									-- Never reached, but nice anyway... (-1, not nice)
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