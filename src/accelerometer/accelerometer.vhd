library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity accelerometer is
--generic (countVal  				 :  INTEGER := 8);
port ( Clk                     : IN    STD_LOGIC;
       sel_axis                : IN    INTEGER range 0 to 2;
       KEY                     : IN    STD_LOGIC;
       G_SENSOR_CS_N           : OUT   STD_LOGIC;
       G_SENSOR_INT            : IN    STD_LOGIC;
       I2C_SCLK                : OUT   STD_LOGIC;
		 G_SENSOR_OUT            : OUT   STD_LOGIC_VECTOR(15 downto 0);
       I2C_SDAT                : INOUT STD_LOGIC );
end entity;
    
architecture accelerometer_arc of accelerometer is

signal dly_rst          : STD_LOGIC;   
signal SPICLK           : STD_LOGIC;   
signal SPICLK_OUT       : STD_LOGIC;   
signal data_acc         : STD_LOGIC_VECTOR(15 downto 0);    
signal G_SENSOR_CS_N_T  : STD_LOGIC;   
signal I2C_SCLK_T       : STD_LOGIC;   


--signal count 				:  INTEGER := 0;
--signal sel_axis_t			:  INTEGER range 0  to 2;
--signal i						:  INTEGER range 0 to 8 := 0;
--signal data_acquire		:  STD_LOGIC := '0';
--signal GSDataOut1 		:  STD_LOGIC_VECTOR(7 downto 0);
--signal GSDataOut2 		:  INTEGER range 0 to 255;


begin
   
   G_SENSOR_CS_N <= G_SENSOR_CS_N_T;
   I2C_SCLK <= I2C_SCLK_T;
--	sel_axis <=	sel_axis_t;
	
   u_reset_delay : entity work.reset_delay 
      port map (
         iRSTN  =>  KEY,
         iCLK   =>  Clk,
         oRST   =>  dly_rst);   
   
   u_spipll : entity work.spipll 
      port map (
         areset  =>  dly_rst,
         inclk0  =>  Clk,
         c0      =>  SPICLK,
         c1      =>  SPICLK_OUT);   
   
   u_spi_ee_config : entity work.spi_ee_config 
      port map (
		   sel_axis     =>  sel_axis,
         iRSTN        =>  NOT dly_rst,
         iSPI_CLK     =>  SPICLK,
         iSPI_CLK_OUT =>  SPICLK_OUT,
         iG_INT2      =>  G_SENSOR_INT,
         oDATA_L      =>  data_acc(7 downto 0),
         oDATA_H      =>  data_acc(15 downto 8),
         SPI_SDIO     =>  I2C_SDAT,
         oSPI_CSN     =>  G_SENSOR_CS_N_T,
         oSPI_CLK     =>  I2C_SCLK_T);
			
--	proc: process(Clk)
--	begin
--		if(rising_edge(Clk)) then -- first if
			
			
		  
--		end if; -- first if end
		
--	end process proc;
	
	G_SENSOR_OUT  <=  data_acc(15 downto 0);
	
end accelerometer_arc;