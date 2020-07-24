// /home/jmonetta/non-rep-software/arduino-1.8.12/hardware/tools/avr/bin/avr-g++ -c -g -Os -w -std=gnu++11 -fpermissive -fno-exceptions -ffunction-sections -fdata-sections -fno-threadsafe-statics -Wno-error=narrowing -MMD -flto -mmcu=atmega328p -DF_CPU=16000000L blink.c -o blink.o
// /home/jmonetta/non-rep-software/arduino-1.8.12/hardware/tools/avr/bin/avr-gcc -w -Os -g -flto -fuse-linker-plugin -Wl,--gc-sections -mmcu=atmega328p -o blink.elf blink.o
// /home/jmonetta/non-rep-software/arduino-1.8.12/hardware/tools/avr/bin/avr-objcopy -O ihex -R .eeprom blink.elf blink.hex
// /home/jmonetta/non-rep-software/arduino-1.8.12/hardware/tools/avr/bin/avr-objdump -m avr -D blink.hex
#include <avr/io.h>
#include <util/delay.h>

int main(void)
{
  DDRC = 0xFF; //Nakes PORTC as Output
  while(1) //infinite loop
    {
      PORTC = 0xFF; //Turns ON All LEDs
      _delay_ms(1000); //1 second delay
      PORTC= 0x00; //Turns OFF All LEDs
      _delay_ms(1000); //1 second delay
    }
}
