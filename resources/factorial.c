// /home/jmonetta/non-rep-software/arduino-1.8.12/hardware/tools/avr/bin/avr-g++ -c -g -O0 -w -std=gnu++11 -fpermissive -fno-exceptions -ffunction-sections -fdata-sections -fno-threadsafe-statics -Wno-error=narrowing -MMD -flto -mmcu=atmega328p -DF_CPU=16000000L factorial.c -o factorial.o
// /home/jmonetta/non-rep-software/arduino-1.8.12/hardware/tools/avr/bin/avr-gcc -w -Os -g -flto -fuse-linker-plugin -Wl,--gc-sections -mmcu=atmega328p -o factorial.elf factorial.o
// /home/jmonetta/non-rep-software/arduino-1.8.12/hardware/tools/avr/bin/avr-objcopy -O ihex -R .eeprom factorial.elf factorial.hex
// /home/jmonetta/non-rep-software/arduino-1.8.12/hardware/tools/avr/bin/avr-objdump -m avr -D factorial.hex


int factorial(int n){
  if (n == 0) {
    return 1;
  } else {
    return n * factorial(n-1);
  }
}

int main(void){
  return factorial(5);
}
