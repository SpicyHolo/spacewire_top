

LIBRARY ieee;
USE ieee.std_logic_1164.ALL, ieee.numeric_std.ALL;
USE work.spwpkg.ALL;
USE work.ALL;

ENTITY
    echotest_send_top IS

    PORT (
        clk50 : IN STD_LOGIC;
        btn_reset : IN STD_LOGIC;
        btn_clear : IN STD_LOGIC;
        switch : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        led : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
        spw_di : IN STD_LOGIC;
        spw_si : IN STD_LOGIC;
        spw_do : OUT STD_LOGIC;
        spw_so : OUT STD_LOGIC);

END ENTITY echotest_send_top;

ARCHITECTURE echotest_send_top_arch OF echotest_send_top IS

    -- Clock generation.
    SIGNAL sysclk : STD_LOGIC;

    -- Synchronize buttons
    SIGNAL s_resetbtn : STD_LOGIC := '0';
    SIGNAL s_clearbtn : STD_LOGIC := '0';

    -- Sticky LED
    SIGNAL s_linkerrorled : STD_LOGIC := '0';

    -- Interface signals.
    --
    -- Config
    SIGNAL s_linkstart : STD_LOGIC := '0';
    SIGNAL s_autostart : STD_LOGIC := '0';
    SIGNAL s_linkdisable : STD_LOGIC := '0';
    SIGNAL s_senddata : STD_LOGIC := '0';
    SIGNAL s_sendtick : STD_LOGIC := '0';
    SIGNAL s_txdivcnt : STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000";
    -- Status
    SIGNAL s_linkstarted : STD_LOGIC;
    SIGNAL s_linkconnecting : STD_LOGIC;
    SIGNAL s_linkrun : STD_LOGIC;
    SIGNAL s_linkerror : STD_LOGIC;
    SIGNAL s_gotdata : STD_LOGIC;
    SIGNAL s_dataerror : STD_LOGIC;
    SIGNAL s_tickerror : STD_LOGIC;
    -- IO
    SIGNAL s_rst : STD_LOGIC := '1';
    SIGNAL s_spwdi : STD_LOGIC;
    SIGNAL s_spwsi : STD_LOGIC;
    SIGNAL s_spwdo : STD_LOGIC;
    SIGNAL s_spwso : STD_LOGIC;
    COMPONENT streamtest IS
        GENERIC (
            sysfreq : real;
            txclkfreq : real;
            tickdiv : INTEGER RANGE 12 TO 24 := 20;
            rximpl : spw_implementation_type := impl_generic;
            rxchunk : INTEGER RANGE 1 TO 4 := 1;
            tximpl : spw_implementation_type := impl_generic;
            rxfifosize_bits : INTEGER RANGE 6 TO 14 := 11;
            txfifosize_bits : INTEGER RANGE 2 TO 14 := 11
        );
        PORT (
            clk : IN STD_LOGIC;
            rxclk : IN STD_LOGIC;
            txclk : IN STD_LOGIC;
            rst : IN STD_LOGIC;
            linkstart : IN STD_LOGIC;
            autostart : IN STD_LOGIC;
            linkdisable : IN STD_LOGIC;
            senddata : IN STD_LOGIC;
            sendtick : IN STD_LOGIC;
            txdivcnt : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
            linkstarted : OUT STD_LOGIC;
            linkconnecting : OUT STD_LOGIC;
            linkrun : OUT STD_LOGIC;
            linkerror : OUT STD_LOGIC;
            gotdata : OUT STD_LOGIC;
            dataerror : OUT STD_LOGIC;
            tickerror : OUT STD_LOGIC;
            spw_di : IN STD_LOGIC;
            spw_si : IN STD_LOGIC;
            spw_do : OUT STD_LOGIC;
            spw_so : OUT STD_LOGIC
        );
    END COMPONENT;

BEGIN

    echotest_send_inst : echotest_send
    GENERIC MAP(
        sysfreq => 50.0e6,
        txclkfreq => 0.0,
        tickdiv => 22,
        rximpl => impl_generic,
        rxchunk => 1,
        tximpl => impl_generic,
        rxfifosize_bits => 11,
        txfifosize_bits => 10)
    PORT MAP(
        clk => sysclk,
        rxclk => '0',
        txclk => '0',
        rst => s_rst,
        linkstart => s_linkstart,
        autostart => s_autostart,
        linkdisable => s_linkdisable,
        senddata => s_senddata,
        sendtick => s_sendtick,
        txdivcnt => s_txdivcnt,
        linkstarted => s_linkstarted,
        linkconnecting => s_linkconnecting,
        linkrun => s_linkrun,
        linkerror => s_linkerror,
        gotdata => s_gotdata,
        dataerror => s_dataerror,
        tickerror => s_tickerror,
        spw_di => s_spwdi,
        spw_si => s_spwsi,
        spw_do => s_spwdo,
        spw_so => s_spwso);

    sysclk <= clk50;
    s_spwdi <= spw_di;
    s_spwsi <= spw_si;
    spw_do <= s_spwdo;
    spw_so <= s_spwso;

    PROCESS (sysclk) IS
    BEGIN
        IF rising_edge(sysclk) THEN

            -- Synchronize buttons
            s_resetbtn <= NOT btn_reset;
            s_rst <= s_resetbtn;
            s_clearbtn <= NOT btn_clear;

            -- Synchronize switch settings
            s_autostart <= '0';
            s_linkstart <= switch(0);
            s_linkdisable <= switch(1);
            s_senddata <= switch(2);
            s_sendtick <= switch(3);
            s_txdivcnt(7 DOWNTO 0) <= "00000000";

            -- Sticky link error LED
            s_linkerrorled <= (s_linkerrorled OR s_linkerror) AND
                (NOT s_clearbtn) AND
                (NOT s_resetbtn);

            -- Drive LEDs (inverted logic)
            led(0) <= s_linkrun;
            led(1) <= s_linkerrorled;
            led(2) <= s_gotdata;
            led(3) <= (s_dataerror OR s_tickerror);

        END IF;
    END PROCESS;

END ARCHITECTURE echotest_send_top_arch;