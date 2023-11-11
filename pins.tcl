#============================================================
# LVDS assignment
#============================================================
set_instance_assignment -name IO_STANDARD LVDS -to spw_do
set_location_assignment PIN_R16 -to spw_do
set_location_assignment PIN_P16 -to "spw_do(n)"

set_instance_assignment -name IO_STANDARD LVDS -to spw_so
set_location_assignment PIN_G15 -to spw_so
set_location_assignment PIN_G16 -to "spw_so(n)"

set_instance_assignment -name IO_STANDARD LVDS -to spw_di
set_location_assignment PIN_B6 -to spw_di
set_location_assignment PIN_A6 -to "spw_di(n)"

set_instance_assignment -name IO_STANDARD LVDS -to spw_si
set_location_assignment PIN_R11 -to spw_si
set_location_assignment PIN_T11 -to "spw_si(n)"

#============================================================
# CLOCKS
#============================================================
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to clk
set_location_assignment PIN_R8 -to clk

#============================================================
# LED - each representing data on the output
#============================================================

# LED[0] s_linkrun
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to led(0)
set_location_assignment PIN_A15 -to led(0)
# LED[1] s_linkerrorled
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to led(1)
set_location_assignment PIN_A13 -to led(1)
# LED[2] s_gotdata
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to led(2)
set_location_assignment PIN_B13 -to led(2)
# LED[3] (s_dataerror or s_tickerror)
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to led(3)
set_location_assignment PIN_A11 -to led(3)


#============================================================
# KEY
#============================================================

# KEY[0] resetbtn
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to button(0)
set_location_assignment PIN_J15 -to button(0)

# KEY[1] clearbtn
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to button(1)
set_location_assignment PIN_E1 -to button(1)

#============================================================
# SWITCH
#============================================================

# SW[0] linkstart
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to switch(0)
set_location_assignment PIN_M1 -to switch(0)

# SW[1] linkdisable
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to switch(1)
set_location_assignment PIN_T8 -to switch(1)

# SW[2] senddata
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to switch(2)
set_location_assignment PIN_B9 -to switch(2)

# SW[3] sendtick
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to switch(3)
set_location_assignment PIN_M15 -to switch(3)

