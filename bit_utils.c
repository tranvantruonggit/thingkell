#include "Rts.h"
#include <stdint.h>
uint32_t weird_adder( uint32_t a, uint32_t b)
{
   return( a + b + 1);
}

uint32_t thk_xor32( uint32_t a, uint32_t b)
{
   return (a^b);
}
uint16_t thk_xor16( uint32_t a, uint32_t b)
{
   return ((a^b) & 0xFFFF);
}
uint8_t thk_xor8( uint32_t a, uint32_t b)
{
   return ((a^b) & 0xFF);
}


uint8_t thk_and8( uint32_t a, uint32_t b)
{
   return ((a&b) & 0xFF);
}

uint16_t thk_and16( uint32_t a, uint32_t b)
{
   return ((a&b) & 0xFFFF);
}

uint32_t thk_and32( uint32_t a, uint32_t b)
{
   return (a&b);
}

extern uint32_t thk_or32( uint32_t a, uint32_t b)
{
   return (a|b);
}

uint8_t thk_shl8( uint32_t a, uint8_t b)
{
   return ((a<<b) & 0xFF);
}

uint16_t thk_shl16( uint32_t a, uint8_t b)
{
   return ((a<<b) & 0xFFFF);
}

uint32_t thk_shl32( uint32_t a, uint8_t b)
{
   return (a<<b);
}
uint32_t thk_shl( uint32_t a, uint8_t b)
{
   return (a<<b);
}

uint32_t thk_shr( uint32_t a, uint8_t b)
{
   return (a>>b);
}

uint32_t thk_tstb( uint32_t val, uint8_t pos)
{
   return ((val >> pos) & 0x1U);
}