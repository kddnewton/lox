# frozen_string_literal: true

module Lox
  module Type
    class Function < Object
      attr_reader :descriptor, :arity, :closure, :callable

      def initialize(descriptor:, arity:, closure:, &callable)
        @descriptor = descriptor
        @arity = arity
        @closure = closure
        @callable = callable
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def call(*arguments)
        callable.call(*arguments)
      end

      def callable?
        true
      end

      def to_lox
        descriptor
      end

      #-------------------------------------------------------------------------
      # Callable methods
      #-------------------------------------------------------------------------

      def ==(other)
        Type.boolean(super)
      end
    end
  end
end
