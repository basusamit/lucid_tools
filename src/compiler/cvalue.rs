//MIT License
//
//Copyright (c) 2019 basusamit
//
//Permission is hereby granted, free of charge, to any person obtaining a copy
//of this software and associated documentation files (the "Software"), to deal
//in the Software without restriction, including without limitation the rights
//to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//copies of the Software, and to permit persons to whom the Software is
//furnished to do so, subject to the following conditions:
//
//The above copyright notice and this permission notice shall be included in all
//copies or substantial portions of the Software.
//
//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//SOFTWARE.

//extern crate num_bigint;

use ndarray::prelude::*;

// It appears a ConstValue can be either
// an array, or a single value.  If it is
// an array, there appears to be a constraint
// that all of the elements are of equal width.
// It appears to be able to hold non-number values
// like "x" and "z".

//pub enum ConstantValue {
//    Unsigned(Vec<num_bigint::BigUint>),
//    Signed(Vec<num_bigint::BigInt>),
//}

use num_traits::Zero;
use std::ops::Add;
use num_bigint::*;
use std::cmp::max;
use ndarray::Zip;
use num_traits::cast::ToPrimitive;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Bit {
    Zero,
    One,
    DontCare,
    HiZ
}

impl Zero for Bit {
    fn zero() -> Self {
        Bit::Zero
    }
    fn is_zero(&self) -> bool {
        return self.eq(&Bit::Zero)
    }
}

impl Add for Bit {
    type Output = Bit;

    // We dont actually have an add operation,
    // but we have a truth table for the Or
    // operation
    fn add(self, other: Bit) -> Bit {
        match self {
            Bit::Zero =>
                other,
            Bit::One =>
                self,
            Bit::DontCare =>
                other,
            Bit::HiZ =>
                other,
        }
    }
}



pub fn bit_xz(a: &Bit) -> bool {
    a.eq(&Bit::DontCare) | a.eq(&Bit::HiZ)
}

fn bit_to_bool(a: &Bit) -> bool {
    match a {
        Bit::Zero => false,
        Bit::One => true,
        _ => false
    }
}

fn bool_to_bit(a: bool) -> Bit {
    match a {
        true => Bit::One,
        false => Bit::Zero
    }
}

// Lucid truth tables:
//
//  localparam P1 = 16'b01xz01xz01xz01xz;
//  localparam P2 = 16'b00001111xxxxzzzz;
//  localparam P3 = 16'b00xx01xxxxxxxxxx; // P1 & P2
pub fn bit_op_and(a: Bit, b: &Bit) -> Bit {
    // if either a or b is a don't care or z, the output is
    // a don't care.  Otherwise, we apply a standard logical and
    if bit_xz(&a) || bit_xz(b) {
        return Bit::DontCare;
    }
    bool_to_bit(bit_to_bool(&a) && bit_to_bool(b))
}

#[test]
fn test_bit_op_and() {
    assert_eq!(bit_op_and(Bit::Zero,&Bit::Zero),Bit::Zero);
    assert_eq!(bit_op_and(Bit::HiZ,&Bit::HiZ),Bit::DontCare);
    assert_eq!(bit_op_and(Bit::DontCare,&Bit::HiZ),Bit::DontCare);
    assert_eq!(bit_op_and(Bit::One,&Bit::HiZ),Bit::DontCare);
    assert_eq!(bit_op_and(Bit::Zero,&Bit::HiZ),Bit::DontCare);
    assert_eq!(bit_op_and(Bit::HiZ,&Bit::DontCare),Bit::DontCare);
    assert_eq!(bit_op_and(Bit::DontCare,&Bit::DontCare),Bit::DontCare);
    assert_eq!(bit_op_and(Bit::One,&Bit::DontCare),Bit::DontCare);
    assert_eq!(bit_op_and(Bit::Zero,&Bit::DontCare),Bit::DontCare);
    assert_eq!(bit_op_and(Bit::HiZ,&Bit::One),Bit::DontCare);
    assert_eq!(bit_op_and(Bit::DontCare,&Bit::One),Bit::DontCare);
    assert_eq!(bit_op_and(Bit::One,&Bit::One),Bit::One);
    assert_eq!(bit_op_and(Bit::Zero,&Bit::One),Bit::Zero);
    assert_eq!(bit_op_and(Bit::HiZ,&Bit::Zero),Bit::DontCare);
    assert_eq!(bit_op_and(Bit::DontCare,&Bit::Zero),Bit::DontCare);
    assert_eq!(bit_op_and(Bit::One,&Bit::Zero),Bit::Zero);
    assert_eq!(bit_op_and(Bit::Zero,&Bit::Zero),Bit::Zero);
}


// Lucid truth tables:
//
//  localparam P1 = 16'b01xz01xz01xz01xz;
//  localparam P2 = 16'b00001111xxxxzzzz;
//  localparam P4 = 16'b01xx1111x1xxx1xx; // P1 | P2
pub fn bit_op_or(a: Bit, b: &Bit) -> Bit {
    // if either a or b is a one, return 1
    if a.eq(&Bit::One) || b.eq(&Bit::One) {
        return Bit::One;
    }
    // Neither is a one.  So if either is a XZ, return a
    // don't care
    if bit_xz(&a) || bit_xz(b) {
        return Bit::DontCare;
    }
    return Bit::Zero;
}

#[test]
fn test_bit_op_or() {
    assert_eq!(bit_op_or(Bit::HiZ, &Bit::HiZ), Bit::DontCare);
    assert_eq!(bit_op_or(Bit::DontCare, &Bit::HiZ), Bit::DontCare);
    assert_eq!(bit_op_or(Bit::One, &Bit::HiZ), Bit::One);
    assert_eq!(bit_op_or(Bit::Zero, &Bit::HiZ), Bit::DontCare);
    assert_eq!(bit_op_or(Bit::HiZ, &Bit::DontCare), Bit::DontCare);
    assert_eq!(bit_op_or(Bit::DontCare, &Bit::DontCare), Bit::DontCare);
    assert_eq!(bit_op_or(Bit::One, &Bit::DontCare), Bit::One);
    assert_eq!(bit_op_or(Bit::Zero, &Bit::DontCare), Bit::DontCare);
    assert_eq!(bit_op_or(Bit::HiZ, &Bit::One), Bit::One);
    assert_eq!(bit_op_or(Bit::DontCare, &Bit::One), Bit::One);
    assert_eq!(bit_op_or(Bit::One, &Bit::One), Bit::One);
    assert_eq!(bit_op_or(Bit::Zero, &Bit::One), Bit::One);
    assert_eq!(bit_op_or(Bit::HiZ, &Bit::Zero), Bit::DontCare);
    assert_eq!(bit_op_or(Bit::DontCare, &Bit::Zero), Bit::DontCare);
    assert_eq!(bit_op_or(Bit::One, &Bit::Zero), Bit::One);
    assert_eq!(bit_op_or(Bit::Zero, &Bit::Zero), Bit::Zero);
}


// Lucid truth tables:
//
//  localparam P1 = 16'b01xz01xz01xz01xz;
//  localparam P2 = 16'b00001111xxxxzzzz;
//  localparam P5 = 16'b01xx1011x1xxx1xx; // P1 ^ P2
pub fn bit_op_xor(a: Bit, b: &Bit) -> Bit {
    // if either a or b is a one, return 1
    if a.eq(&Bit::One) ^ b.eq(&Bit::One) {
        return Bit::One;
    }
    // Neither is a one.  So if either is a XZ, return a
    // don't care
    if bit_xz(&a) || bit_xz(b) {
        return Bit::DontCare;
    }
    return Bit::Zero;
}

#[test]
fn test_bit_op_xor() {
    assert_eq!(bit_op_xor(Bit::HiZ,&Bit::HiZ),Bit::DontCare);
    assert_eq!(bit_op_xor(Bit::DontCare,&Bit::HiZ),Bit::DontCare);
    assert_eq!(bit_op_xor(Bit::One,&Bit::HiZ),Bit::One);
    assert_eq!(bit_op_xor(Bit::Zero,&Bit::HiZ),Bit::DontCare);
    assert_eq!(bit_op_xor(Bit::HiZ,&Bit::DontCare),Bit::DontCare);
    assert_eq!(bit_op_xor(Bit::DontCare,&Bit::DontCare),Bit::DontCare);
    assert_eq!(bit_op_xor(Bit::One,&Bit::DontCare),Bit::One);
    assert_eq!(bit_op_xor(Bit::Zero,&Bit::DontCare),Bit::DontCare);
    assert_eq!(bit_op_xor(Bit::HiZ,&Bit::One),Bit::One);
    assert_eq!(bit_op_xor(Bit::DontCare,&Bit::One),Bit::One);
    assert_eq!(bit_op_xor(Bit::One,&Bit::One),Bit::Zero);
    assert_eq!(bit_op_xor(Bit::Zero,&Bit::One),Bit::One);
    assert_eq!(bit_op_xor(Bit::HiZ,&Bit::Zero),Bit::DontCare);
    assert_eq!(bit_op_xor(Bit::DontCare,&Bit::Zero),Bit::DontCare);
    assert_eq!(bit_op_xor(Bit::One,&Bit::Zero),Bit::One);
    assert_eq!(bit_op_xor(Bit::Zero,&Bit::Zero),Bit::Zero);
}

// Lucid truth tables:
//
//  localparam P7 = 4'b01xz;
//  localparam P8 = 4'b10xz;
pub fn bit_op_not(a: &Bit) -> Bit {
    if bit_xz(a) {
        a.clone()
    } else {
        bool_to_bit(!bit_to_bool(a))
    }
}

#[test]
fn test_bit_not() {
    assert_eq!(bit_op_not(&Bit::Zero),Bit::One);
    assert_eq!(bit_op_not(&Bit::One),Bit::Zero);
    assert_eq!(bit_op_not(&Bit::DontCare),Bit::DontCare);
    assert_eq!(bit_op_not(&Bit::HiZ),Bit::HiZ);
}

// Lucid truth tables:
//
//  localparam P1 = 16'b01xz01xz01xz01xz;
//  localparam P2 = 16'b00001111xxxxzzzz;
//  localparam P6 = 16'b10xx0100x0xxx0xx; P1 ~^ P2
pub fn bit_op_xnor(a: Bit, b: &Bit) -> Bit {
    bit_op_not(&bit_op_xor(a,b))
}

#[test]
fn test_bit_op_xnor() {
    assert_eq!(bit_op_xnor(Bit::HiZ,&Bit::HiZ),Bit::DontCare);
    assert_eq!(bit_op_xnor(Bit::DontCare,&Bit::HiZ),Bit::DontCare);
    assert_eq!(bit_op_xnor(Bit::One,&Bit::HiZ),Bit::Zero);
    assert_eq!(bit_op_xnor(Bit::Zero,&Bit::HiZ),Bit::DontCare);
    assert_eq!(bit_op_xnor(Bit::HiZ,&Bit::DontCare),Bit::DontCare);
    assert_eq!(bit_op_xnor(Bit::DontCare,&Bit::DontCare),Bit::DontCare);
    assert_eq!(bit_op_xnor(Bit::One,&Bit::DontCare),Bit::Zero);
    assert_eq!(bit_op_xnor(Bit::Zero,&Bit::DontCare),Bit::DontCare);
    assert_eq!(bit_op_xnor(Bit::HiZ,&Bit::One),Bit::Zero);
    assert_eq!(bit_op_xnor(Bit::DontCare,&Bit::One),Bit::Zero);
    assert_eq!(bit_op_xnor(Bit::One,&Bit::One),Bit::One);
    assert_eq!(bit_op_xnor(Bit::Zero,&Bit::One),Bit::Zero);
    assert_eq!(bit_op_xnor(Bit::HiZ,&Bit::Zero),Bit::DontCare);
    assert_eq!(bit_op_xnor(Bit::DontCare,&Bit::Zero),Bit::DontCare);
    assert_eq!(bit_op_xnor(Bit::One,&Bit::Zero),Bit::Zero);
    assert_eq!(bit_op_xnor(Bit::Zero,&Bit::Zero),Bit::One);
}


#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ConstantValue {
    pub value: ArrayD<Bit>,
    pub signed: Sign,
}

impl ConstantValue {
    pub fn empty() -> ConstantValue {
        ConstantValue {
            value: Array::zeros(vec![]),
            signed: Sign::Plus,
        }
    }
    pub fn one_dim(count : usize) -> ConstantValue {
        ConstantValue {
            value: Array::zeros(vec![count]),
            signed: Sign::Plus,
        }
    }

    pub fn from_bool(b: bool) -> ConstantValue {
        let result =  if b {
            BigInt::from(1)
        } else {
            BigInt::from(0)
        };
        ConstantValue::from_bigint(&result)
    }

    pub fn from_bigint(bigint: &BigInt) -> ConstantValue {
        // We need to convert this to a constant value
        let mut ret = ConstantValue::one_dim(max(1,bigint.bits()));
        // This is the binary representation of the value
        let (sign,bin_rep) = bigint.to_radix_be(2);
        for digit in 0..bin_rep.len() {
            match bin_rep[digit] {
                0 => ret.value[digit] = Bit::Zero,
                1 => ret.value[digit] = Bit::One,
                _ => ret.value[digit] = Bit::DontCare,
            }
        }
        ret.signed = sign;
        ret
    }

    pub fn from_bit(bit: &Bit) -> ConstantValue {
        let mut ret = ConstantValue::one_dim(1);
        ret.signed = Sign::Plus;
        ret.value[0] = *bit;
        ret
    }

    pub fn from_bitvec(bits: &[Bit]) -> ConstantValue {
        ConstantValue {
            value: ArrayD::<Bit>::from_shape_vec(vec![bits.len()], bits.to_vec()).unwrap(),
            signed: Sign::Plus,
        }
    }

    pub fn select(&self, ndx: BigInt) -> ConstantValue {
        // The index is little endian
        let n = ndx.to_u32().unwrap() as usize;
        let m = self.value.len();
        ConstantValue::from_bit(&self.value[m - 1 - n])
    }

    pub fn slice(&self, ndx: BigInt) -> ConstantValue {
        if self.value.ndim() == 1 {
            return self.select(ndx);
        }
        let last_axis = self.value.ndim()-1;
        let index = ndx.to_u32().unwrap() as usize;
        let view = self.value.index_axis(Axis(0), index);
        let value = view.to_owned();
        println!("Slice: \nBase {:?},\n index {:?}\n view {:?}\n value {:?}\n", self.value, index, view, value);
        ConstantValue {
            value,
            signed: self.signed
        }
    }

    pub fn bitwise(&self, other: &ConstantValue, func: &Fn(Bit, &Bit) -> Bit) -> ConstantValue {
        let mut res = ArrayD::<Bit>::zeros(self.value.dim());
        Zip::from(&mut res).and(&self.value).and(&other.value).apply(|a, b, c| {
            *a = func(*b,&c);
        });
        ConstantValue {
            value: res,
            signed: self.signed
        }
    }

    pub fn map(&self, func: fn(&Bit) -> Bit) -> ConstantValue {
        ConstantValue {
            value: self.value.map(func),
            signed: self.signed
        }
    }

    pub fn fold(&self, init: Bit, func: &Fn(Bit, &Bit) -> Bit) -> ConstantValue {
        let res = self.value.fold(init, func);
        ConstantValue::from_bit(&res)
    }

    pub fn is_number(&self) -> bool {
        if self.value.ndim() != 1 {
            return false;
        }
        self.value.iter().fold(true, |flag, x| flag && (x.eq(&Bit::One) || x.eq(&Bit::Zero)))
    }

    pub fn as_int(&self) -> BigInt {
        if !self.is_number() {
            panic!("attempt to convert non-number into bigint");
        }
        let binary = self.value.iter()
            .map(|x|
                match x {
                    &Bit::Zero => 0,
                    &Bit::One => 1,
                    _ => panic!("Unexpected bit")
                })
            .collect::<Vec<u8>>();
        BigInt::from_radix_be(self.signed,&binary,2).unwrap()
    }

    pub fn flatten(&self) -> ConstantValue {
        let vec = self.value.iter()
            .map(|x| x.clone())
            .collect::<Vec<Bit>>();
        let value = ArrayD::<Bit>::from_shape_vec(vec![vec.len()], vec).unwrap();
        ConstantValue {
            value,
            signed: self.signed
        }
    }
}
