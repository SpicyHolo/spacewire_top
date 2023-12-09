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
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

-- The entity of a Terasic DE0-board.
entity spacewire_lcd_driver is
	port (CLOCK_50 : in std_logic; -- DE0 CLOCK_50 (50MHz CLK)
			KEY      : in std_logic_vector(1 downto 0); -- DE0 KEY (button) [reset]
			LED      : out std_logic_vector(7 downto 0) := (others => '0'); -- DE0 LEDs [LED0 is 1 when writing a char]
			
			
			-- External LCD ports
			LCD_EN   : out std_logic;
			LCD_RS   : out std_logic;
			LCD_RW   : out std_logic;
			LCD_DATA : inout std_logic_vector(7 downto 0)
	);
	
end entity spacewire_lcd_driver;

-- The architecture!
architecture hardware of spacewire_lcd_driver is

-- Adds empty characters at the end of string, to 
function stringPadding(str : string; length : natural) return string is
	variable paddedString : string(1 to length) := (others => ' ');
	begin
		paddedString(1 to str'length) := str;
		return paddedString;
end function;

-- Data converting function

-- accel_data is 16 bit vector (2s Complement) 
-- function converts it to a REAL value
-- than converts it to a string to the given decimal point precision
-- Returns a formatted string with a prefix and suffix.
-- Make sure your string won't exceed 20 characters (16 due to bug with LCD display)

-- "selected range" is the value range chosen on the accelerometer, default is +/- 2g, for correct interpretation of data
function convertData(accel_data: STD_LOGIC_VECTOR(15 downto 0);
							prefix: string;
							suffix: string; 
							selected_range: INTEGER; 
							precision: natural) return STRING is
	variable accel_value : REAL;
	variable accel_value_str : STRING(1 to 20);
	variable temp_string : STRING(1 to 20); -- Adjust the length based on your needs
begin
	 -- Convert to REAL, scaled for correct interpretation (check accelerometer datasheet)
    accel_value := REAL(TO_INTEGER(SIGNED(accel_data))) * (REAL(selected_range) / 2.0**12.0);
	 
	 -- Convert REAL to string, add padding to 20 to avoid length errors
    accel_value_str := stringPadding(real'image(accel_value), 20);
	 
	 -- Return formatted string, with correct decimal point precision
	 return stringPadding(prefix & accel_value_str(1 to (precision+2)) & suffix, 20);
	 
end function;

-- Component declaration of the LCD module
component lcd_driver_hd44780_module is
	generic (freq         : integer := 50000000;
				areset_pol   : std_logic := '1';
				time_init1   : time := 40 ms;
				time_init2   : time := 4100 us;
				time_init3   : time := 100 us;
				time_tas     : time := 60 ns;
				time_cycle_e : time := 1000 ns;
				time_pweh    : time := 500 ns;
				time_no_bf   : time := 2 ms;
				cursor_on    : boolean := false;
				blink_on     : boolean := false;
				use_bf       : boolean := true
			  );
	port	  (clk      : in std_logic;
			   areset   : in std_logic;
			   -- User site
			   init     : in std_logic;
  			   data     : in std_logic_vector(7 downto 0);
			   wr       : in std_logic;
			   cls      : in std_logic;
			   home     : in std_logic;
			   goto10   : in std_logic;
			   goto20   : in std_logic;
			   goto30   : in std_logic;
			   busy     : out std_logic;
			   -- LCD side
			   LCD_E    : out std_logic;
			   LCD_RS   : out std_logic;
			   LCD_RW   : out std_logic;
			   LCD_DB   : inout std_logic_vector(7 downto 0)
			  );
end component lcd_driver_hd44780_module;

-- The system's frequency
constant sys_freq : integer := 50000000;

signal areset   : std_logic;
signal clk      : std_logic;
signal init     : std_logic;
signal data     : std_logic_vector(7 downto 0);
signal wr       : std_logic;
signal cls      : std_logic;
signal home     : std_logic;
signal goto10   : std_logic;
signal goto20   : std_logic;
signal goto30   : std_logic;
signal busy		 : std_logic;

type state_type is (reset, write_char, write_char_wait, update, update_linecount,
						  update_linecount_wait, write_char_1, write_char_1_wait,
						  write_char_2, write_char_2_wait, write_char_3, write_char_4, hold);
signal state : state_type;

-- A string of 20 characters
subtype string20_type is string(1 to 20);
-- An array of 4 strings of 20 characters.
type message4x20_type is array (1 to 4) of string20_type;

-- The four-line message
constant message : message4x20_type :=
							( 1 => stringPadding("Spacewire", 20),
							  2 => convertData("1101101111011011", "x: ", "g", 2, 4),
							  3 => convertData("0011100100101010", "    y: ", "g", 2, 4),
							  4 => convertData("1111110000101110", "    z: ", "g", 2, 4));

-- Counts the characters on a line.
signal character_counter : integer range 1 to 20;
-- Counts the lines.
signal line_counter : integer range 1 to 4;

begin

	-- Push buttons are active low.
	areset <= not KEY(0);

	-- The clock
	clk <= CLOCK_50;
	
	-- Use LCD module.
	lcdm : lcd_driver_hd44780_module
	generic map (freq => sys_freq, areset_pol => '1', time_cycle_e => 2000 ns, time_pweh => 500 ns,
					 cursor_on => false, blink_on => false, use_bf => false)
	port map (clk => clk, areset => areset, init => init, data => data, wr => wr, cls => cls,
				 home => home, goto10 => goto10, goto20 => goto20, goto30 => goto30, busy => busy,
				 LCD_E => LCD_EN, LCD_RS => LCD_RS, LCD_RW => LCD_RW, LCD_DB => LCD_DATA);
				 
	-- The client side
	drive: process (clk, areset) is
	variable aline : string20_type;
	begin
		if areset = '1' then
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
		elsif rising_edge(clk) then
			wr <= '0';
			init <= '0';
			cls <= '0';
			home <= '0';
			goto10 <= '0';
			goto20 <= '0';
			goto30 <= '0';
			LED(0) <= '0';
			data <= "00000000";
			
			
			case state is

				when reset =>
					-- Wait for the LCD module ready
					if busy = '0' then
						state <= write_char;
					end if;
					-- Setup message counter, start at 1.
					character_counter <= 1;
					line_counter <= 1;
					
				when write_char =>
					LED(0) <= '1';
					-- Set up WRITE!
					-- Use the data from the string
					aline := message(line_counter);
					data <= std_logic_vector( to_unsigned( character'pos(aline(character_counter)),8));
 					wr <= '1';
					state <= write_char_wait;

				when write_char_wait =>
					-- This state is needed so that the LCD driver
					-- can process the write command. Note that data
					-- and wr are registered outputs and get their
					-- respective values while in *this* state. If you don't
					-- want this behaviour, please make your outputs
					-- non-registered.
					state <= update;
					
				when update =>
					LED(0) <= '1';
					-- Wait for the write complete
					if busy = '0' then
						-- If end of string, goto hold mode...
						if line_counter = 4 and character_counter = 20 then
							state <= hold;
						-- If end of line...	
						elsif character_counter = 20 then
							case line_counter is
								when 1 => goto10 <= '1';
								when 2 => goto20 <= '1';
								when 3 => goto30 <= '1';
								-- Never reached, but nice anyway...
								when 4 => home <= '1';
								when others => null;
							end case;
							-- Set new values of the counters
							line_counter <= line_counter+1;
							character_counter <= 1;
							-- Goto the update state
							state <= update_linecount;
						else
						   -- Not the end of a lines, update the character counter.
							character_counter <= character_counter+1;
							state <= write_char;
						end if;
					end if;
				
				when update_linecount =>
					-- This state is needed so that the LCD driver
					-- can process the gotoXX command. Note that the gotoXX
					-- signals are registered outputs and get their
					-- respective values while in *this* state. If you don't
					-- want this behaviour, please make your outputs
					-- non-registered.
					state <= update_linecount_wait;
					
				when update_linecount_wait =>
					-- Wait for the LCD module ready
					if busy = '0' then
						state <= write_char;
					end if;
				
				-- The "hohouwer"
				when hold =>
					state <= hold;
					
				when others =>
					null;

			end case;
		end if;
	end process;
end architecture hardware;