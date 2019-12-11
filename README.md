# A small letter parser

The idea is that you provide some text consting of a-i characters, for example:
	
	abc fg  hij abaa ab ab
	
The letters will be replaced by what you fill in the inputs for a,b,c,d etc..

For example, we can set it to generate some chuck function calls:

a = play(100);\n
b = play(200);\n
c = play(300);\n
d = play(400);\n
e = play(500);\n
f = play(600);\n
etc..

To play back this score of function calls, you need to combine it with a very small program:

SinOsc c => dac;
c.gain(0.25);

fun void play(float x) {
	x => c.freq;
	second / x => now;
}

This will play a sinewave segment for 1 cycle of the waveform.

You can also play around with random values:

play(Math.random2(100,1000));

Save if with .ck extension, you can run it by pasting it in a miniaudicle window, and clicking the green plus (make sure you start the virtual machine before you do this, keyboard shortcut is __Cmd/Ctrl+ [ . ]__ ).

Chuck can be downloaded [here](https://chuck.cs.princeton.edu/release/).





