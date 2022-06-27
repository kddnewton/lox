# frozen_string_literal: true

module Lox
  module Type
    # This represents a string.
    class String < Object
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
        value
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def +(other)
        String.new(value: value + other.unwrap_string)
      end

      def ==(other)
        Type.boolean((other in String[value: ^(value)]))
      end
    end
  end
end
