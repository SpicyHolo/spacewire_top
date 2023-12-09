LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY accelerometer IS
   PORT (
      clk : IN STD_LOGIC;
      sel_axis : IN INTEGER RANGE 0 TO 2;
      rst : IN STD_LOGIC;
      G_SENSOR_CS_N : OUT STD_LOGIC;
      G_SENSOR_INT : IN STD_LOGIC;
      I2C_SCLK : OUT STD_LOGIC;
      G_SENSOR_OUT : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
      I2C_SDAT : INOUT STD_LOGIC);
END ENTITY;

ARCHITECTURE accelerometer_arch OF accelerometer IS
   SIGNAL dly_rst : STD_LOGIC;
   SIGNAL SPICLK : STD_LOGIC;
   SIGNAL SPICLK_OUT : STD_LOGIC;
   SIGNAL data_acc : STD_LOGIC_VECTOR(15 DOWNTO 0);
   SIGNAL G_SENSOR_CS_N_T : STD_LOGIC;
   SIGNAL I2C_SCLK_T : STD_LOGIC;
BEGIN

   G_SENSOR_CS_N <= G_SENSOR_CS_N_T;
   I2C_SCLK <= I2C_SCLK_T;

   u_reset_delay : ENTITY work.reset_delay
      PORT MAP(
         iRSTN => rst,
         iCLK => clk,
         oRST => dly_rst
      );

   u_spipll : ENTITY work.spipll
      PORT MAP(
         areset => dly_rst,
         inclk0 => clk,
         c0 => SPICLK,
         c1 => SPICLK_OUT
      );

   u_spi_ee_config : ENTITY work.spi_ee_config
      PORT MAP(
         sel_axis => sel_axis,
         iRSTN => NOT dly_rst,
         iSPI_CLK => SPICLK,
         iSPI_CLK_OUT => SPICLK_OUT,
         iG_INT2 => G_SENSOR_INT,
         oDATA_L => data_acc(7 DOWNTO 0),
         oDATA_H => data_acc(15 DOWNTO 8),
         SPI_SDIO => I2C_SDAT,
         oSPI_CSN => G_SENSOR_CS_N_T,
         oSPI_CLK => I2C_SCLK_T
      );


   G_SENSOR_OUT <= data_acc(15 DOWNTO 0);

END accelerometer_arch;