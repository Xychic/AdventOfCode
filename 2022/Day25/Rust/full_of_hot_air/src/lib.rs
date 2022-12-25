use core::fmt::Debug;
use std::{
    collections::VecDeque,
    iter::Sum,
    ops::{Add, AddAssign},
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum SNAFUDigit {
    DoubleMinus,
    Minus,
    Zero,
    One,
    Two,
}

impl SNAFUDigit {
    fn to_decimal(&self) -> isize {
        match self {
            Self::DoubleMinus => -2,
            Self::Minus => -1,
            Self::Zero => 0,
            Self::One => 1,
            Self::Two => 2,
        }
    }

    fn from_decimal(c: &char) -> Self {
        match c {
            '=' => Self::DoubleMinus,
            '-' => Self::Minus,
            '0' => Self::Zero,
            '1' => Self::One,
            '2' => Self::Two,
            _ => panic!("Invalid digit {c}"),
        }
    }
}

impl Debug for SNAFUDigit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Zero => write!(f, "0"),
            Self::One => write!(f, "1"),
            Self::Two => write!(f, "2"),
            Self::Minus => write!(f, "-"),
            Self::DoubleMinus => write!(f, "="),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct SNAFU {
    data: VecDeque<SNAFUDigit>,
}

impl SNAFU {
    fn new(str: &str) -> Self {
        Self {
            data: str.chars().map(|c| SNAFUDigit::from_decimal(&c)).collect(),
        }
    }

    fn len(&self) -> usize {
        self.data.len()
    }
}

impl Debug for SNAFU {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.data
                .iter()
                .map(|d| format!("{d:?}"))
                .collect::<String>()
        )
    }
}

impl Add for SNAFU {
    type Output = SNAFU;

    fn add(self, rhs: Self) -> Self::Output {
        let self_len = self.len();
        let rhs_len = rhs.len();
        let len = self_len.max(rhs_len);

        let mut ans = VecDeque::with_capacity(len);

        let mut carry = 0;

        let mut digit_sum;
        for i in 1..=len {
            digit_sum = carry;
            if i <= self_len {
                digit_sum += self.data[self_len - i].to_decimal();
            }
            if i <= rhs_len {
                digit_sum += rhs.data[rhs_len - i].to_decimal();
            }

            carry = 0;

            while digit_sum >= 3 {
                carry += 1;
                digit_sum -= 5
            }
            while digit_sum <= -3 {
                carry -= 1;
                digit_sum += 5;
            }
            assert!(-2 <= digit_sum && digit_sum <= 2);
            ans.push_front(SNAFUDigit::from_decimal(&match digit_sum {
                -2 => '=',
                -1 => '-',
                0 => '0',
                1 => '1',
                _ => '2',
            }));
        }

        while carry > 0 {
            digit_sum = carry;
            carry = 0;
            while digit_sum >= 3 {
                carry += 1;
                digit_sum -= 5
            }
            while digit_sum <= -3 {
                carry -= 1;
                digit_sum += 5;
            }
            assert!(-2 <= digit_sum && digit_sum <= 2);
            ans.push_front(SNAFUDigit::from_decimal(&match digit_sum {
                -2 => '=',
                -1 => '-',
                0 => '0',
                1 => '1',
                _ => '2',
            }));
        }

        SNAFU { data: ans }
    }
}

impl AddAssign for SNAFU {
    fn add_assign(&mut self, rhs: Self) {
        *self = self.clone() + rhs;
    }
}

impl Sum for SNAFU {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut ans = SNAFU::new("0");
        for num in iter {
            ans += num;
        }
        ans
    }
}

type Input = Vec<SNAFU>;

pub fn parse(input: &str) -> Input {
    input.trim().lines().map(|l| SNAFU::new(l)).collect()
}

pub fn part_1(input: &Input) -> String {
    format!("{:?}", input.iter().cloned().sum::<SNAFU>())
}
