#include <stdint.h>

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

uint32_t thk_inv8( uint32_t val)
{
   return ((~val)&0xFFU);
}

uint32_t thk_inv16( uint32_t val)
{
   return ((~val)&0xFFFFU);
}


uint32_t thk_inv32( uint32_t val)
{
   return ((~val));
}

uint32_t thk_bit_reversal( uint32_t val, uint8_t bits)
{
    val = ((val & 0x55555555) << 1) | ((val & 0xAAAAAAAA) >> 1); // Swap _<>_
    val = ((val & 0x33333333) << 2) | ((val & 0xCCCCCCCC) >> 2); // Swap __<>__
    val = ((val & 0x0F0F0F0F) << 4) | ((val & 0xF0F0F0F0) >> 4); // Swap ____<>____
    val = ((val & 0x00FF00FF) << 8) | ((val & 0xFF00FF00) >> 8); // Swap ...
    val = ((val & 0x0000FFFF) << 16) | ((val & 0xFFFF0000) >> 16); // Swap ...
    return (val >> (32 - bits));
}

uint32_t thk_bit_reflect8( uint8_t val )
{
    uint32_t temp;
    temp = val;
    return (thk_bit_reversal(val, 8));
}

uint32_t thk_bit_reflect16( uint16_t val )
{
    uint32_t temp;
    temp = val;
    return (thk_bit_reversal(val, 16));
}

uint32_t thk_bit_reflect32( uint32_t val )
{
    uint32_t temp;
    temp = val;
    return (thk_bit_reversal(val, 32));
}