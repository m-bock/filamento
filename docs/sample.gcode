; --- Simple 3 mm-high circle, R = 50 mm ---
G21             ; units = mm
G90             ; absolute coords
M82             ; absolute extrusion

;—heat & home (adjust temps as desired)—
M140 S60        ; bed → 60 °C
M190 S60        ; wait bed
M104 S200       ; nozzle → 200 °C
M109 S200       ; wait nozzle
G28             ; home all axes

;—move up to 3 mm layer height—
G1 Z0.200 F1200  ; move to first layer height (0.2 mm) F1200

;—go to circle start point (150,100)—
G1 X150 Y100 F3000

;—prime a bit—
G92 E0
G1 E5 F300

;—purge/wipe line at side—
; Move to purge start at X10,Y10
G1 X10 Y10 F3000
; Extrude while moving to X100,Y10
G1 X100 Y10 E10 F1000
; Retract slightly
G1 E8 F300


;—draw CW circle of R=50 mm around center (100,100)—
;  start at (150,100), I = –50, J = 0
G2 X150 Y100 I-50 J0 E314.16 F1000

;—finish—
M104 S0         ; turn off nozzle
M140 S0         ; turn off bed
M84             ; motors off
