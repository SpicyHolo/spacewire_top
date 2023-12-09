

library ieee;
use ieee.std_logic_1164.all, ieee.numeric_std.all;
use work.spwpkg.all;
use work.all;

entity echotest_recv_top is

    port (
        clk50:        in  std_logic;
        btn_reset:  in  std_logic;
        btn_clear:  in  std_logic;
        switch:     in  std_logic_vector(3 downto 0);
        led:        out std_logic_vector(7 downto 0);
        spw_di:     in  std_logic;
        spw_si:     in  std_logic;
        spw_do:     out std_logic;
        spw_so:     out std_logic );

end entity echotest_recv_top;

architecture echotest_recv_top_arch of echotest_recv_top is

    -- Clock generation.
    signal sysclk:          std_logic;

    -- Synchronize buttons
    signal s_resetbtn:      std_logic := '0';
    signal s_clearbtn:      std_logic := '0';

    -- Sticky LED
    signal s_linkerrorled:  std_logic_vector(4 downto 0) := "00000";

    -- Interface signals.
    signal s_rst:           std_logic := '1';
    signal s_linkstart:     std_logic := '0';
    signal s_autostart:     std_logic := '0';
    signal s_linkdisable:   std_logic := '0';
    signal s_senddata:      std_logic := '0';
    signal s_sendtick:      std_logic := '0';
    signal s_txdivcnt:      std_logic_vector(7 downto 0) := "00000000";
    signal s_linkstarted:   std_logic;
    signal s_linkconnecting: std_logic;
    signal s_linkrun:       std_logic;
    signal s_linkerror:     std_logic;
    signal s_gotdata:       std_logic;
    signal s_dataerror:     std_logic;
    signal s_tickerror:     std_logic;
    signal s_spwdi:         std_logic;
    signal s_spwsi:         std_logic;
    signal s_spwdo:         std_logic;
    signal s_spwso:         std_logic;

    signal s_linkerror_disc:     std_logic;
    signal s_linkerror_par:     std_logic;
    signal s_linkerror_esc:     std_logic;
    signal s_linkerror_cred:     std_logic;


    component echotest_recv is
        generic (
            sysfreq:    real;
            txclkfreq:  real;
            tickdiv:    integer range 12 to 24 := 20;
            rximpl:     spw_implementation_type := impl_generic;
            rxchunk:    integer range 1 to 4 := 1;
            tximpl:     spw_implementation_type := impl_generic;
            rxfifosize_bits: integer range 6 to 14 := 11;
            txfifosize_bits: integer range 2 to 14 := 11
        );
        port (
            clk:        in  std_logic;
            rxclk:      in  std_logic;
            txclk:      in  std_logic;
            rst:        in  std_logic;
            linkstart:  in  std_logic;
            autostart:  in  std_logic;
            linkdisable: in std_logic;
            senddata:   in  std_logic;
            sendtick:   in  std_logic;
            txdivcnt:   in  std_logic_vector(7 downto 0);
            linkstarted: out std_logic;
            linkconnecting: out std_logic;
            linkrun:    out std_logic;
            linkerror:  out std_logic;
            gotdata:    out std_logic;
            dataerror:  out std_logic;
            tickerror:  out std_logic;
            spw_di:     in  std_logic;
            spw_si:     in  std_logic;
            spw_do:     out std_logic;
            spw_so:     out std_logic;
            linkerror_disc: out std_logic;
            linkerror_par: out std_logic;
            linkerror_esc: out std_logic;
            linkerror_cred: out std_logic
        );
    end component;

begin

    echotest_recv_inst: echotest_recv
        generic map (
            sysfreq     => 50.0e6,
            txclkfreq   => 0.0,
            tickdiv     => 22,
            rximpl      => impl_generic,
            rxchunk     => 1,
            tximpl      => impl_generic,
            rxfifosize_bits => 11,
            txfifosize_bits => 10
        )
        port map (
            clk         => sysclk,
            rxclk       => '0',
            txclk       => '0',
            rst         => s_rst,
            linkstart   => s_linkstart,
            autostart   => s_autostart,
            linkdisable => s_linkdisable,
            senddata    => s_senddata,
            sendtick    => s_sendtick,
            txdivcnt    => s_txdivcnt,
            linkstarted => s_linkstarted,
            linkconnecting => s_linkconnecting,
            linkrun     => s_linkrun,
            linkerror   => s_linkerror,
            gotdata     => s_gotdata,
            dataerror   => s_dataerror,
            tickerror   => s_tickerror,
            spw_di      => s_spwdi,
            spw_si      => s_spwsi,
            spw_do      => s_spwdo,
            spw_so      => s_spwso,
            linkerror_disc => s_linkerror_disc,
            linkerror_par => s_linkerror_par,
            linkerror_esc => s_linkerror_esc,
            linkerror_cred => s_linkerror_cred
        );
				
	sysclk <= clk50;
	s_spwdi <= spw_di;
    s_spwsi <= spw_si;
    spw_do <= s_spwdo;
    spw_so <= s_spwso;

    process (sysclk) is
    begin
        if rising_edge(sysclk) then

            -- Synchronize buttons
            s_resetbtn  <= not btn_reset;
            s_rst       <= s_resetbtn;
            s_clearbtn  <= not btn_clear;

            -- Synchronize switch settings
            s_autostart <= '0';
            s_linkstart <= switch(0);
            s_linkdisable <= switch(1);
            s_senddata  <= switch(2);
            s_sendtick  <= switch(3);
            s_txdivcnt(7 downto 0) <= "00000000";

            -- Sticky link error LED
            s_linkerrorled(0) <= (s_linkerrorled(0) or s_linkerror) and
                (not s_clearbtn) and (not s_resetbtn);

            s_linkerrorled(1) <= (s_linkerrorled(1) or s_linkerror_disc) and
                (not s_clearbtn) and (not s_resetbtn);

            s_linkerrorled(2) <= (s_linkerrorled(2) or s_linkerror_par) and
                (not s_clearbtn) and (not s_resetbtn);

            s_linkerrorled(3) <= (s_linkerrorled(3) or s_linkerror_esc) and
                (not s_clearbtn) and (not s_resetbtn);

            s_linkerrorled(4) <= (s_linkerrorled(4) or s_linkerror_cred) and
                (not s_clearbtn) and (not s_resetbtn);

                              

            -- Drive LEDs (inverted logic)
            led(0)  <= s_linkrun;
            led(1)  <= s_linkerrorled(0);
            led(2)  <= s_linkerrorled(1);
            led(3)  <= s_linkerrorled(2);
            led(4)  <= s_linkerrorled(3);
            led(5)  <= s_linkerrorled(4);
            led(6)  <= s_gotdata;
			led(7)  <= s_tickerror;

        end if;
    end process;

end architecture echotest_recv_top_arch;