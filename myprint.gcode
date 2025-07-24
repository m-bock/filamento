Build profile: -w ghc-9.4.8 -O1
In order, the following will be built (use -v for more details):
 - jee-code-0.1.0.0 (exe:jee-code-exe) (file app/Main.hs changed)
Preprocessing executable 'jee-code-exe' for jee-code-0.1.0.0...
Building executable 'jee-code-exe' for jee-code-0.1.0.0...
[1 of 1] Compiling Main             ( app/Main.hs, /home/m/dev/jee-code/dist-newstyle/build/x86_64-linux/ghc-9.4.8/jee-code-0.1.0.0/x/jee-code-exe/build/jee-code-exe/jee-code-exe-tmp/Main.o ) [Source file changed]
[2 of 2] Linking /home/m/dev/jee-code/dist-newstyle/build/x86_64-linux/ghc-9.4.8/jee-code-0.1.0.0/x/jee-code-exe/build/jee-code-exe/jee-code-exe [Objects changed]
M300 S1000 P500 ; beep
G21             ; units = mm
G90             ; absolute coords
M82             ; absolute extrusion
; —heat & home (adjust temps as desired)—
M140 S60        ; bed → 60 °C
M190 S60        ; wait bed
M104 S200       ; nozzle → 200 °C
M109 S200       ; wait nozzle
; —move up to first layer height—
G1 X0.0 Y0.0 F3000
G1 X100.0 Y100.0 F3000
M300 S2000 P500 ; beep

