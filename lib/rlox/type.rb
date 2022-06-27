# frozen_string_literal: true

module Lox
  module Type
    # The way lox works according to the book, you can call all of the operators
    # on any kind of value. So here we're going to prepare base implementations
    # that just raise.
    class Object
      def unwrap_number(message = "Operands must be numbers.")
        raise Error::RuntimeError, message unless (self in Number)
        value
      end

      def unwrap_string(message = "Operands must be two numbers or two strings.")
        raise Error::RuntimeError, message unless (self in String)
        value
      end

      def -@() = raise Error::RuntimeError, "Operand must be a number."
      def +(other) = raise Error::RuntimeError, "Operands must be two numbers or two strings."
      def -(other) = raise Error::RuntimeError, "Operands must be numbers."
      def *(other) = raise Error::RuntimeError, "Operands must be numbers."
      def /(other) = raise Error::RuntimeError, "Operands must be numbers."
      def <(other) = raise Error::RuntimeError, "Operands must be numbers."
      def <=(other) = raise Error::RuntimeError, "Operands must be numbers."
      def >(other) = raise Error::RuntimeError, "Operands must be numbers."
      def >=(other) = raise Error::RuntimeError, "Operands must be numbers."

      def !=(other)
        !(self == other)
      end
    end

    # A convenient shortcut to turn a Ruby boolean value into a lox boolean
    # value.
    def self.boolean(value)
      value ? True.instance : False.instance
    end
  end
end
