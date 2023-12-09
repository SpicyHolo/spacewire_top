--controlling and configuring a SPI 

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY spi_ee_config IS
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
      sel_axis : IN INTEGER RANGE 0 TO 2;
      iRSTN : IN STD_LOGIC;
      iSPI_CLK : IN STD_LOGIC;
      iSPI_CLK_OUT : IN STD_LOGIC;
      iG_INT2 : IN STD_LOGIC;
      oDATA_L : OUT STD_LOGIC_VECTOR(SO_DataL DOWNTO 0);
      oDATA_H : OUT STD_LOGIC_VECTOR(SO_DataL DOWNTO 0);
      SPI_SDIO : INOUT STD_LOGIC;
      oSPI_CSN : OUT STD_LOGIC;
      oSPI_CLK : OUT STD_LOGIC);
END spi_ee_config;

ARCHITECTURE spi_ee_config_arc OF spi_ee_config IS

   COMPONENT spi_controller
      GENERIC (
         X_LB : STD_LOGIC_VECTOR(5 DOWNTO 0) := "110010";
         Z_HB : STD_LOGIC_VECTOR(5 DOWNTO 0) := "110111";
         -- Data MSB Bit
         -- Initial Reg Number 
         Y_LB : STD_LOGIC_VECTOR(5 DOWNTO 0) := "110100";
         READ_MODE : STD_LOGIC_VECTOR(1 DOWNTO 0) := "10";
         Z_LB : STD_LOGIC_VECTOR(5 DOWNTO 0) := "110110";
         SI_DataL : INTEGER := 15;
         THRESH_ACT : STD_LOGIC_VECTOR(5 DOWNTO 0) := "100100";
         THRESH_INACT : STD_LOGIC_VECTOR(5 DOWNTO 0) := "100101";
         POWER_CONTROL : STD_LOGIC_VECTOR(5 DOWNTO 0) := "101101";
         SO_DataL : INTEGER := 7;
         INT_ENABLE : STD_LOGIC_VECTOR(5 DOWNTO 0) := "101110";
         THRESH_FF : STD_LOGIC_VECTOR(5 DOWNTO 0) := "101000";
         -- SPI State 
         TIME_FF : STD_LOGIC_VECTOR(5 DOWNTO 0) := "101001";
         TIME_INACT : STD_LOGIC_VECTOR(5 DOWNTO 0) := "100110";
         TRANSFER : STD_LOGIC := '1';
         ACT_INACT_CTL : STD_LOGIC_VECTOR(5 DOWNTO 0) := "100111";
         DATA_FORMAT : STD_LOGIC_VECTOR(5 DOWNTO 0) := "110001";
         X_HB : STD_LOGIC_VECTOR(5 DOWNTO 0) := "110011";
         -- Write/Read Mode 
         -- Read Reg Address
         -- Write Reg Address 
         INT_MAP : STD_LOGIC_VECTOR(5 DOWNTO 0) := "101111";
         Y_HB : STD_LOGIC_VECTOR(5 DOWNTO 0) := "110101");
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
   END COMPONENT;

   SIGNAL ini_index : STD_LOGIC_VECTOR(3 DOWNTO 0);
   --current index for configuration settings.
   SIGNAL write_data : STD_LOGIC_VECTOR(SI_DataL - 2 DOWNTO 0);
   --
   SIGNAL p2s_data : STD_LOGIC_VECTOR(SI_DataL DOWNTO 0);
   --for transferring data from the entity to the spi_controller component
   SIGNAL spi_go : STD_LOGIC;
   --signal indicating when to initiate a SPI transfer.
   SIGNAL spi_end : STD_LOGIC;

   SIGNAL s2p_data : STD_LOGIC_VECTOR(SO_DataL DOWNTO 0);
   --for data transferred from the spi_controller component to the entity
   SIGNAL low_byte_data : STD_LOGIC_VECTOR(SO_DataL DOWNTO 0);

   SIGNAL spi_state : STD_LOGIC;

   SIGNAL high_byte : STD_LOGIC;
   --control signal indicating whether the higher byte of data is being processed
   SIGNAL read_back : STD_LOGIC;

   SIGNAL clear_status : STD_LOGIC;

   SIGNAL read_ready : STD_LOGIC;

   SIGNAL clear_status_d : STD_LOGIC_VECTOR(3 DOWNTO 0);
   --storing the clear status control signal, delayed by one clock cycle
   SIGNAL high_byte_d : STD_LOGIC;
   --delayed version of the high_byte control signal
   SIGNAL read_back_d : STD_LOGIC;

   SIGNAL read_idle_count : STD_LOGIC_VECTOR(IDLE_MSB DOWNTO 0);

   SIGNAL oDATA_L_xhdl1 : STD_LOGIC_VECTOR(SO_DataL DOWNTO 0);
   --The lower byte of data transferred from the spi_controller to the entity
   SIGNAL oDATA_H_xhdl2 : STD_LOGIC_VECTOR(SO_DataL DOWNTO 0);

   SIGNAL oSPI_CSN_xhdl3 : STD_LOGIC;
   --The chip select signal transferred from the spi_controller to the entity
   SIGNAL oSPI_CLK_xhdl4 : STD_LOGIC;
   --The clock signal transferred from the spi_controller to the entity.
BEGIN

   oDATA_L <= oDATA_L_xhdl1;
   oDATA_H <= oDATA_H_xhdl2;
   oSPI_CSN <= oSPI_CSN_xhdl3;
   oSPI_CLK <= oSPI_CLK_xhdl4;

   u_spi_controller : spi_controller
   PORT MAP(
      iRSTN => iRSTN,
      iSPI_CLK => iSPI_CLK,
      iSPI_CLK_OUT => iSPI_CLK_OUT,
      iP2S_DATA => p2s_data,
      iSPI_GO => spi_go,
      oSPI_END => spi_end,
      oS2P_DATA => s2p_data,
      SPI_SDIO => SPI_SDIO,
      oSPI_CSN => oSPI_CSN_xhdl3,
      oSPI_CLK => oSPI_CLK_xhdl4);

   PROCESS (ini_index)
   BEGIN
      CASE ini_index IS
         WHEN "0000" =>
            write_data <= THRESH_ACT & "00100000";
         WHEN "0001" =>
            write_data <= THRESH_INACT & "00000011";
         WHEN "0010" =>
            write_data <= TIME_INACT & "00000001";
         WHEN "0011" =>
            write_data <= ACT_INACT_CTL & "01111111";
         WHEN "0100" =>
            write_data <= THRESH_FF & "00001001";
         WHEN "0101" =>
            write_data <= TIME_FF & "01000110";
         WHEN "0110" =>
            write_data <= BW_RATE & "00001001";
         WHEN "0111" =>
            write_data <= INT_ENABLE & "00010000";
         WHEN "1000" =>
            write_data <= INT_MAP & "00010000";
         WHEN "1001" =>
            write_data <= DATA_FORMAT & "01000000";
         WHEN OTHERS =>
            write_data <= POWER_CONTROL & "00001000";

      END CASE;
   END PROCESS;

   PROCESS (iRSTN, iSPI_CLK, sel_axis)
   BEGIN
      IF (iRSTN = '0') THEN
         ini_index <= "0000";
         spi_go <= '0';
         spi_state <= IDLE;
         read_idle_count <= (OTHERS => '0');
         high_byte <= '0';
         read_back <= '0';
         clear_status <= '0';
      ELSIF rising_edge(iSPI_CLK) THEN
         IF (ini_index < INI_NUMBER) THEN
            CASE spi_state IS
               WHEN IDLE =>
                  p2s_data <= WRITE_MODE & write_data;
                  spi_go <= '1';
                  spi_state <= TRANSFER;
               WHEN TRANSFER =>
                  IF (spi_end = '1') THEN
                     ini_index <= ini_index + "0001";
                     spi_go <= '0';
                     spi_state <= IDLE;
                  END IF;
               WHEN OTHERS =>
                  NULL;

            END CASE;
         ELSE
            CASE spi_state IS
               WHEN IDLE =>
                  read_idle_count <= read_idle_count + "000000000000001";
                  IF (high_byte = '1') THEN
                     IF (sel_axis = 0) THEN
                        p2s_data(15 DOWNTO 8) <= READ_MODE & X_HB;
                     ELSIF (sel_axis = 1) THEN
                        p2s_data(15 DOWNTO 8) <= READ_MODE & Y_HB;
                     ELSIF (sel_axis = 2) THEN
                        p2s_data(15 DOWNTO 8) <= READ_MODE & Z_HB;
                     END IF;
                     read_back <= '1';
                  ELSE
                     IF (read_ready = '1') THEN
                        IF (sel_axis = 0) THEN
                           p2s_data(15 DOWNTO 8) <= READ_MODE & X_LB;
                        ELSIF (sel_axis = 1) THEN
                           p2s_data(15 DOWNTO 8) <= READ_MODE & Y_LB;
                        ELSIF (sel_axis = 2) THEN
                           p2s_data(15 DOWNTO 8) <= READ_MODE & Z_LB;
                        END IF;
                        read_back <= '1';
                     ELSE
                        IF (((NOT clear_status_d(3) AND iG_INT2) OR read_idle_count(IDLE_MSB)) = '1') THEN
                           p2s_data(15 DOWNTO 8) <= READ_MODE & INT_SOURCE;
                           clear_status <= '1';
                        END IF;
                     END IF;
                  END IF;
                  IF ((high_byte OR read_ready OR read_idle_count(IDLE_MSB) OR (NOT clear_status_d(3) AND iG_INT2)) = '1') THEN
                     spi_go <= '1';
                     spi_state <= TRANSFER;
                  END IF;
                  IF (read_back_d = '1') THEN
                     IF (high_byte_d = '1') THEN
                        oDATA_H_xhdl2 <= s2p_data;
                        oDATA_L_xhdl1 <= low_byte_data;
                     ELSE
                        low_byte_data <= s2p_data;
                     END IF;
                  END IF;
               WHEN TRANSFER =>
                  IF (spi_end = '1') THEN
                     spi_go <= '0';
                     spi_state <= IDLE;
                     IF (read_back = '1') THEN
                        read_back <= '0';
                        high_byte <= NOT high_byte;
                        read_ready <= '0';
                     ELSE
                        clear_status <= '0';
                        read_ready <= s2p_data(6);
                        read_idle_count <= (OTHERS => '0');
                     END IF;
                  END IF;
               WHEN OTHERS =>
                  NULL;

            END CASE;
         END IF;
      END IF;
   END PROCESS;

   PROCESS (iRSTN, iSPI_CLK)
   BEGIN
      IF (iRSTN = '0') THEN
         high_byte_d <= '0';
         read_back_d <= '0';
         clear_status_d <= "0000";
      ELSIF (rising_edge(iSPI_CLK)) THEN
         high_byte_d <= high_byte;
         read_back_d <= read_back;
         clear_status_d <= clear_status_d(2 DOWNTO 0) & clear_status;
      END IF;
   END PROCESS;

END spi_ee_config_arc;