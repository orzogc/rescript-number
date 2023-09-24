include ResNumber__Others

module type Number = ResNumber__Number.Number

module type Integer = ResNumber__Integer.Integer

module type SignedInteger = ResNumber__Integer.SignedInteger

module type UnsignedInteger = ResNumber__Integer.UnsignedInteger

module type Float = ResNumber__Float.Float

module MakeInteger = ResNumber__Integer.MakeInteger

module MakeSignedInteger = ResNumber__Integer.MakeSignedInteger

module MakeUnsignedInteger = ResNumber__Integer.MakeUnsignedInteger

module Int8: SignedInteger = ResNumber__Integer.Int8

module Uint8: UnsignedInteger = ResNumber__Integer.Uint8

module Int16: SignedInteger = ResNumber__Integer.Int16

module Uint16: UnsignedInteger = ResNumber__Integer.Uint16

module Int32: SignedInteger = ResNumber__Integer.Int32

module Uint32: UnsignedInteger = ResNumber__Integer.Uint32

module Float64: Float = ResNumber__Float.Float64
