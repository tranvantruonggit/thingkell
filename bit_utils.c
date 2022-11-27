#include "Rts.h"
#include <stdint.h>
uint32_t weird_adder( uint32_t a, uint32_t b)
{
   return( a + b + 1);
}

extern uint32_t thk_xor32( uint32_t a, uint32_t b)
{
   return (a^b);
}
extern uint16_t thk_xor16( uint32_t a, uint32_t b)
{
   return ((a^b) & 0xFFFF);
}
extern uint8_t thk_xor8( uint32_t a, uint32_t b)
{
   return ((a^b) & 0xFF);
}


extern uint8_t thk_and8( uint32_t a, uint32_t b)
{
   return ((a&b) & 0xFF);
}
   
extern uint16_t thk_and16( uint32_t a, uint32_t b)
{
   return ((a&b) & 0xFFFF);
   
}

extern uint32_t thk_and32( uint32_t a, uint32_t b)
{
   
   return (a&b);
}
