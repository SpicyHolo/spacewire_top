--This process essentially implements a counter or shift register 
--that delays the reset signal by a certain number of clock cycles. 
--The delayed reset is then available at the output port oRST.

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY reset_delay IS
   PORT (
      iRSTN : IN STD_LOGIC; --input reset signal
      iCLK : IN STD_LOGIC; --input port for the clock signal
      oRST : OUT STD_LOGIC); --output port for the delayed reset signal 
END reset_delay;

ARCHITECTURE reset_delay_arc OF reset_delay IS

   SIGNAL cont : STD_LOGIC_VECTOR(20 DOWNTO 0); --shift register
   SIGNAL oRST_T : STD_LOGIC; --inside signal for deleyed reset

BEGIN

   oRST <= oRST_T;

   rst_proc : PROCESS (iCLK, iRSTN)
   BEGIN
      IF (iRSTN = '0') THEN
         cont <= "000000000000000000000";
         oRST_T <= '1';
      ELSIF (rising_edge(iCLK)) THEN
         IF (NOT cont(20) = '1') THEN
            cont <= cont + "000000000000000000001";
            oRST_T <= '1';
         ELSE
            oRST_T <= '0';
         END IF;
      END IF;
   END PROCESS rst_proc;

END reset_delay_arc;