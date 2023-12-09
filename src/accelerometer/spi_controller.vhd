LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
USE IEEE.STD_LOGIC_MISC.ALL;

ENTITY spi_controller IS
   GENERIC (
      -- Data MSB Bit
      IDLE_MSB : INTEGER := 14;
      SI_DataL : INTEGER := 15;
      SO_DataL : INTEGER := 7;
      -- Write/Read Mode 
      WRITE_MODE : STD_LOGIC_VECTOR(1 DOWNTO 0) := "00";
      READ_MODE : STD_LOGIC_VECTOR(1 DOWNTO 0) := "10";
      -- Initial Reg Number 
      INI_NUMBER : STD_LOGIC_VECTOR(3 DOWNTO 0) := "1011";
      -- SPI State 
      IDLE : STD_LOGIC := '0';
      TRANSFER : STD_LOGIC := '1';
      -- Write Reg Address 
      BW_RATE : STD_LOGIC_VECTOR(5 DOWNTO 0) := "101100";
      POWER_CONTROL : STD_LOGIC_VECTOR(5 DOWNTO 0) := "101101";
      DATA_FORMAT : STD_LOGIC_VECTOR(5 DOWNTO 0) := "110001";
      INT_ENABLE : STD_LOGIC_VECTOR(5 DOWNTO 0) := "101110";
      INT_MAP : STD_LOGIC_VECTOR(5 DOWNTO 0) := "101111";
      THRESH_ACT : STD_LOGIC_VECTOR(5 DOWNTO 0) := "100100";
      THRESH_INACT : STD_LOGIC_VECTOR(5 DOWNTO 0) := "100101";
      TIME_INACT : STD_LOGIC_VECTOR(5 DOWNTO 0) := "100110";
      ACT_INACT_CTL : STD_LOGIC_VECTOR(5 DOWNTO 0) := "100111";
      THRESH_FF : STD_LOGIC_VECTOR(5 DOWNTO 0) := "101000";
      TIME_FF : STD_LOGIC_VECTOR(5 DOWNTO 0) := "101001";
      -- Read Reg Address
      INT_SOURCE : STD_LOGIC_VECTOR(5 DOWNTO 0) := "110000";
      X_LB : STD_LOGIC_VECTOR(5 DOWNTO 0) := "110010";
      X_HB : STD_LOGIC_VECTOR(5 DOWNTO 0) := "110011";
      Y_LB : STD_LOGIC_VECTOR(5 DOWNTO 0) := "110100";
      Y_HB : STD_LOGIC_VECTOR(5 DOWNTO 0) := "110101";
      Z_LB : STD_LOGIC_VECTOR(5 DOWNTO 0) := "110110";
      Z_HB : STD_LOGIC_VECTOR(5 DOWNTO 0) := "110111");
   PORT (
      iRSTN : IN STD_LOGIC;
      iSPI_CLK : IN STD_LOGIC;
      iSPI_CLK_OUT : IN STD_LOGIC;
      iP2S_DATA : IN STD_LOGIC_VECTOR(SI_DataL DOWNTO 0);
      iSPI_GO : IN STD_LOGIC;
      oSPI_END : OUT STD_LOGIC;
      oS2P_DATA : OUT STD_LOGIC_VECTOR(SO_DataL DOWNTO 0);
      SPI_SDIO : INOUT STD_LOGIC;
      oSPI_CSN : OUT STD_LOGIC;
      oSPI_CLK : OUT STD_LOGIC);
END spi_controller;

ARCHITECTURE spi_controller_arc OF spi_controller IS

   SIGNAL read_mode_xhdl5 : STD_LOGIC;
   SIGNAL write_address : STD_LOGIC;
   SIGNAL spi_count_en : STD_LOGIC;
   SIGNAL spi_count : STD_LOGIC_VECTOR(3 DOWNTO 0);
   SIGNAL temp_xhdl6 : STD_LOGIC;
   SIGNAL temp_xhdl7 : STD_LOGIC;
   SIGNAL oSPI_END_xhdl1 : STD_LOGIC;
   SIGNAL oS2P_DATA_xhdl2 : STD_LOGIC_VECTOR(SO_DataL DOWNTO 0);
   SIGNAL oSPI_CSN_xhdl3 : STD_LOGIC;
   SIGNAL oSPI_CLK_xhdl4 : STD_LOGIC;

BEGIN
   oSPI_END <= oSPI_END_xhdl1;
   oS2P_DATA <= oS2P_DATA_xhdl2;
   oSPI_CSN <= oSPI_CSN_xhdl3;
   oSPI_CLK <= oSPI_CLK_xhdl4;
   read_mode_xhdl5 <= iP2S_DATA(SI_DataL);
   write_address <= spi_count(3);
   oSPI_END_xhdl1 <= NOR_REDUCE(spi_count);
   oSPI_CSN_xhdl3 <= NOT iSPI_GO;
   temp_xhdl6 <= iSPI_CLK_OUT WHEN spi_count_en = '1' ELSE
      '1';
   oSPI_CLK_xhdl4 <= temp_xhdl6;
   temp_xhdl7 <= iP2S_DATA(conv_integer(spi_count)) WHEN (spi_count_en AND (NOT read_mode_xhdl5 OR write_address)) = '1' ELSE
      'Z';
   SPI_SDIO <= temp_xhdl7;

   PROCESS (iRSTN, iSPI_CLK)
   BEGIN
      IF (iRSTN = '0') THEN
         spi_count_en <= '0';
         spi_count <= "1111";
      ELSIF rising_edge(iSPI_CLK) THEN
         IF (oSPI_END_xhdl1 = '1') THEN
            spi_count_en <= '0';
         ELSE
            IF (iSPI_GO = '1') THEN
               spi_count_en <= '1';
            END IF;
         END IF;
         IF (NOT spi_count_en = '1') THEN
            spi_count <= "1111";
         ELSE
            spi_count <= spi_count - "0001";
         END IF;
         IF ((read_mode_xhdl5 AND NOT write_address) = '1') THEN
            oS2P_DATA_xhdl2 <= oS2P_DATA_xhdl2(SO_DataL - 1 DOWNTO 0) & SPI_SDIO;
         END IF;
      END IF;
   END PROCESS;

END spi_controller_arc;