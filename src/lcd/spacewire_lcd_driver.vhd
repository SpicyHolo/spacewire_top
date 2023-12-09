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
		KEY : IN STD_LOGIC; -- DE0 KEY (button) [reset]
		LED : OUT STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0'); -- DE0 LEDs [LED0 is 1 when writing a char]
		-- External LCD ports
		LCD_EN : OUT STD_LOGIC;
		LCD_RS : OUT STD_LOGIC;
		LCD_RW : OUT STD_LOGIC;
		LCD_DATA : INOUT STD_LOGIC_VECTOR(7 DOWNTO 0);

		-- LCD Register control
		lcd_register_data_in: IN STD_LOGIC_VECTOR(15 DOWNTO 0)
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

	-- TODO
	-- In any measurement range, the 10-bit resolution can be selected. The 10-bit acceleration value can be adjusted left or right. 
	-- If the value is right-adjusted, the 10-bit acceleration values can be obtained by masking the second byte 
	-- (which is read from 0x33 for the x-axis, 0x35 for the y-axis, and 0x37 for the z-axis) — with 0x03, 
	-- right-shifting it eight times. Then, the two bytes are combined (0x32 and 0x33; 0x34 and 0x35; 0x36 and 0x37) 
	-- to a 16-bit integer. 

	-- If the value is adjusted left, the 10-bit acceleration values can be obtained by right-shifting the first byte 
	-- (which is read from 0x32 for the x-axis, 0x34 for the y-axis, and 0x36 for the z-axis) six times, masking the second byte 
	-- (which is read from 0x33 for the x-axis, 0x35 for the y-axis, and 0x37 for the z-axis) with 0x3F to a temporary byte. 
	-- Its temporary byte is left-shifted two times, adding the temporary byte to the first byte. 
	-- Then, left-shift the first byte six times and combine the two bytes (0x32 and 0x33; 0x34 and 0x35; 0x36; and 0x37) for a 16-bit integer.
	
	-- This 10-bit value will range from 0 to 1024. The acceleration is measured in both directions along the axis. 
	-- So, if the value is greater than 511, subtract 1024 from it to get a negative value which indicates the other direction of the axis. 
	
	-- For a 10-bit resolution, the value of the acceleration in a gravity unit can be derived by multiplying this value with 4 mg (0.004) for +/- 2g range, 7.8 mg (0.0078) 
	-- for +/- 4g range, 15.6 mg (0.0156) for +/- 8g, or 31.25 mg (0.03125) for +/- 16g range. 

	-- TODO Not sure of the conversion, check where is padding, and if the scale is correct
	FUNCTION convertData(accel_data : STD_LOGIC_VECTOR(15 DOWNTO 0);
		prefix : STRING;
		suffix : STRING;
		selected_range : INTEGER;
		precision : NATURAL) RETURN STRING IS
		VARIABLE accel_value : INTEGER;
		VARIABLE accel_value_str : STRING(1 TO 20);
		VARIABLE temp_string : STRING(1 TO 20); -- Adjust the length based on your needs
	BEGIN
		-- Convert to REAL, scaled for correct interpretation (check accelerometer datasheet)
		accel_value := TO_INTEGER(SIGNED(accel_data)); -- TODO fix this xD

		-- Convert REAL to string, add padding to 20 to avoid length errors
		accel_value_str := stringPadding(integer'image(accel_value), 20);

		-- Return formatted string, with correct decimal point precision
		RETURN stringPadding(prefix & accel_value_str(1 TO 6) & suffix, 20);

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
		update_linecount_wait, hold, update_home, update_home_wait);
	SIGNAL state : state_type;

	-- A string of 20 characters
	SUBTYPE string20_type IS STRING(1 TO 20);
	-- An array of 4 strings of 20 characters.
	TYPE message4x20_type IS ARRAY (1 TO 4) OF string20_type;
	CONSTANT empty_message : message4x20_type := (
		1 => stringPadding("No Data", 20),
		2 => convertData("0000000000000000", "x: ", "g", 2, 4),
		3 => convertData("0000000000000000", "    y: ", "g", 2, 4),
		4 => convertData("0000000000000000", "    z: ", "g", 2, 4)
	);
	CONSTANT debug_message : message4x20_type := (
		1 => stringPadding("Debug", 20),
		2 => convertData("0111111111111111", "x: ", "g", 2, 4),
		3 => convertData("0000000000000000", "    y: ", "g", 2, 4),
		4 => convertData("0111111111111111", "    z: ", "g", 2, 4)
	);
	SIGNAL message : message4x20_type := empty_message; 

	function vector_to_string(v : std_logic_vector(15 downto 0)) return string is
        variable result : string(v'length downto 1);
    begin
        for i in v'range loop
            result(i) := character'val(to_integer(v(i)));
        end loop;
        return result;
    end function;

	-- Counts the characters on a line.
	SIGNAL character_counter : INTEGER RANGE 1 TO 20;
	-- Counts the lines.
	SIGNAL line_counter : INTEGER RANGE 1 TO 4;
	SIGNAL got_nonzero_data : STD_LOGIC := '0';

BEGIN

	-- Push buttons are active low.
	areset <= NOT KEY;

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
		VARIABLE msg : message4x20_type;
		VARIABLE bit_string : string(16 downto 1);
		VARIABLE padded_bit_string : string(20 downto 1);
		VARIABLE lcd_register_capture : std_logic_vector(15 downto 0);
	BEGIN
		msg := message;
		IF areset = '1' THEN
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
			-- LED <= (others => '0');
			wr <= '0';
			init <= '0';
			cls <= '0';
			home <= '0';
			goto10 <= '0';
			goto20 <= '0';
			goto30 <= '0';
			data <= "00000000";
			LED <= lcd_register_data_in(7 DOWNTO 0);
			--wchodzimy na pewno do ifa, ale nie wyswietlaja sie poprawne wartosci na ekranie, mimo ze na ledach tak
			IF TO_INTEGER(SIGNED(lcd_register_data_in)) /= 0 THEN
				lcd_register_capture := lcd_register_data_in;
				bit_string := vector_to_string(lcd_register_capture);
				padded_bit_string := "    " & bit_string;
				message <= (
					1 => stringPadding("Good Data", 20),
					2 => stringPadding(integer'image(TO_INTEGER(SIGNED(lcd_register_data_in))), 20),
					3 => convertData("0111111111111111", "    const", "g", 2, 4),
					4 => padded_bit_string
				);
				--message <= debug_message;
				got_nonzero_data <= NOT got_nonzero_data;
				-- LED(7) <= got_nonzero_data;
			ELSE
				message <=  empty_message;
			END IF;
			CASE state IS

				WHEN reset => -- Initial state
					-- LED(0) <= '1';
					-- Wait for the LCD module ready
					IF busy = '0' THEN -- TODO check if this will be saved between processes
						--convertData(lcd_register_data_in, "x: ", "g", 2, 4);
						-- message <= empty_message;
						-- message <= (
						-- 	1 => stringPadding("    Spacewire", 20),
						-- 	2 => convertData(lcd_register_data_in, "x: ", "g", 2, 4),
						-- 	3 => convertData("0000000000000000", "    y: ", "g", 2, 4),
						-- 	4 => convertData("0000000000000000", "    z: ", "g", 2, 4)
						-- );
						
						state <= write_char;
					END IF;
					-- Setup message counter, start at 1.
					character_counter <= 1;
					line_counter <= 1;

				WHEN write_char =>
					-- LED(1) <= '1';
					-- Set up WRITE!
					-- Use the data from the string
					aline := msg(line_counter);
					data <= STD_LOGIC_VECTOR(to_unsigned(CHARACTER'pos(aline(character_counter)), 8));
					wr <= '1';
					state <= write_char_wait;

				WHEN write_char_wait =>
					-- LED(2) <= '1';
					-- This state is needed so that the LCD driver
					-- can process the write command. Note that data
					-- and wr are registered outputs and get their
					-- respective values while in *this* state. If you don't
					-- want this behaviour, please make your outputs
					-- non-registered.
					state <= update;

				WHEN update =>
					-- LED(3) <= '1';
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
					-- LED(4) <= '1';
					-- This state is needed so that the LCD driver
					-- can process the gotoXX command. Note that the gotoXX
					-- signals are registered outputs and get their
					-- respective values while in *this* state. If you don't
					-- want this behaviour, please make your outputs
					-- non-registered.
					state <= update_linecount_wait;

				WHEN update_linecount_wait =>
					-- LED(5) <= '1';
					-- Wait for the LCD module ready
					IF busy = '0' THEN
						state <= write_char;
					END IF;
					-- The "hohouwer" (hangover of santa claus +1) +1
					
				WHEN hold =>
					-- LED(6) <= '1';
					home <= '1';
					state <= update_home;

				WHEN update_home =>
					state <= update_home_wait;

				WHEN update_home_wait =>
					IF busy = '0' THEN
						state <= reset;
					END IF;
			END CASE;
		END IF;
	END PROCESS;
END ARCHITECTURE hardware;