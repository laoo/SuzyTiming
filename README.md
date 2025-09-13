# SuzyTiming

Simple yet powerful Suzy timing test.

It just performs two linked SCB paintings: drawing of 102x102 16 color sprite and then clearing it with background color.

What's important is that just befor giving the bus to the Suzy a timer is set up to the amount of microseconds displayed in upper-left corner. On the interrupt routine the painting buffer is copied to display buffer after which Suzy is allowed to paint the rest of the sprites.

In the effect on the display shows up the amount of the image Suzy was able to paint in given number of microseconds.

The number of microseconds can be changed using d-pad.
