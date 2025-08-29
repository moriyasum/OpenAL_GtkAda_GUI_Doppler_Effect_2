# OpenAL_GtkAda_GUI_Doppler_Effect_2

This program is a GtkAda application that demonstrates the Doppler effect using OpenAL-GtkAda.

Moving airplane causes Stereo Sound Doppler effect.

GTKAda: Adacore 2020, Source Airplane moving in the screen and the Listner location is relocatable.
OpenAL: Coreland 1.1.1, Stereo Doppler effect, Soure: 1000Hz PCM Mono tone sound file.
Timer Interrupt: Locate next position, Cycle: Programmable.
X,Y Location: Calculation X=Sin(2*Pi*t)+X0, Y=Sin(2*Pi*t)+Y0.
Doppler Effect: OpenAL calculates with relative speed.

Updated 8/29/2025: Bug fixed and Warnings fixed.

Tested environement:
Ubuntu: 24.04.2 LTS, Gnat: 14.2.1, Gtkada:25.0.1
OpenAL-Ada(Coreland) 1.1.1, Alire 2.0.2
PC: OMEN 17-ck2095cl, i9-13900HX, RTX-4080
Audio source file: Mono, 44.1kHz, 16bit, 5sec WAV file
<img width="1100" height="750" alt="gui_doppler_plane_1_screenshot" src="https://github.com/user-attachments/assets/659bc34e-b787-44db-a46f-28dd6257ad90" />

Detail: https://n7sd.com/ada1-2/openal-with-gtkada-gui-sample2/
