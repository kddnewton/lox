# frozen_string_literal: true

module Lox
  module Type
    # This represents a class.
    class Class < Object
      attr_reader :name, :methods

      def initialize(name:, methods:)
        @name = name
        @methods = methods
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def arity
        find_method("init")&.arity || 0
      end

      def call(*arguments)
        instance = Instance.new(klass: self)
        if (method = find_method("init"))
          method.bind(instance).call(*arguments)
        end

        instance
      end

      def callable?
        true
      end

      def find_method(name)
        methods[name]
      end

      def to_lox
        name
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
