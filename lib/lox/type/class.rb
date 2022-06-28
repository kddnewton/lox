# frozen_string_literal: true

module Lox
  module Type
    # This represents a class.
    class Class < Object
      attr_reader :name

      def initialize(name:)
        @name = name
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def arity
        0
      end

      def call
        Instance.new(klass: self)
      end

      def callable?
        true
      end

      def to_lox
        name
      end

      #-------------------------------------------------------------------------
      # Callable methods
      #-------------------------------------------------------------------------

      def ==(other)
        Type.boolean((other in Class[name: ^(name)]))
      end
    end
  end
end
