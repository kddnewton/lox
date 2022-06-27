# frozen_string_literal: true

module Lox
  module Type
    # This represents a number. Lox makes no delineation between integers/
    # floats/bignums/etc.
    class Number < Object
      attr_reader :value

      def initialize(value:)
        @value = value
      end

      #-------------------------------------------------------------------------
      # Debug methods
      #-------------------------------------------------------------------------

      def deconstruct_keys(keys)
        { value: value }
      end

      def to_lox
        sign = value.sign >= 0 ? "" : "-"
        nums = value % 1 == 0 ? value.abs.to_i : value.abs.to_s("F")
        "#{sign}#{nums}"
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def -@
        Number.new(value: -value)
      end

      def +(other) = Number.new(value: value + other.unwrap_number("Operands must be two numbers or two strings."))
      def -(other) = Number.new(value: value - other.unwrap_number)
      def *(other) = Number.new(value: value * other.unwrap_number)
      def /(other) = Number.new(value: value / other.unwrap_number)

      def <(other) = Type.boolean(value < other.unwrap_number)
      def <=(other) = Type.boolean(value <= other.unwrap_number)
      def >(other) = Type.boolean(value > other.unwrap_number)
      def >=(other) = Type.boolean(value >= other.unwrap_number)

      def ==(other) = Type.boolean((other in Number[value: ^(value)]))
    end
  end
end
