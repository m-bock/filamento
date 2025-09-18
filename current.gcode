G21                                                      ; [ init ] Set units to millimeters
M83                                                      ; [ init ] Set extruder to relative mode
M140       S65                                           ; [ init ] Set bed temperature
M104      S205                                           ; [ init ] Set hotend temperature
G28                                                      ; [ init / autoHome ] Auto home axes
M190       S65                                           ; [ init ] Wait for bed temperature
M109      S205                                           ; [ init ] Wait for hotend temperature
                                                         ; Split below
M140       S65                                           ; [ filament / Filament Change ] Set bed temperature
M104      S205                                           ; [ filament / Filament Change ] Set hotend temperature
G1     E0.0000     F9000 X110.0000   Y5.0000  Z20.0000   ; [ filament / Filament Change ] Linear move
M190       S65                                           ; [ filament / Filament Change ] Wait for bed temperature
M109      S205                                           ; [ filament / Filament Change ] Wait for hotend temperature
M300      P500      S880                                 ; [ filament / Filament Change ] Play tone
