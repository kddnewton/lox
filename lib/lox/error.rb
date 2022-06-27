# frozen_string_literal: true

module Lox
  # This module contains the definitions of any of the various errors that can
  # occur when parsing and evaluating a lox program.
  module Error
    # This is the parent class of any kind of error in our program.
    class Error < StandardError
    end

    # This error occurs during parsing when a token is not recognized or not
    # properly formed.
    class SyntaxError < Error
      EXIT_CODE = 65

      attr_reader :location

      def initialize(message, location = nil)
        super(message)
        @location = location
      end

      def detailed_message(source: nil, **)
        return super unless source

        lineno = source[0..location.start].count("\n") + 1
        "[line #{lineno}] #{message}"
      end
    end

    # This is an error that occurs when a lox error is raised.
    class RuntimeError < Error
      EXIT_CODE = 70

      attr_reader :location

      def initialize(message, location = nil)
        super(message)
        @location = location
      end

      def detailed_message(source: nil, **)
        return super unless source

        lineno = source[0..location.start].count("\n") + 1
        "#{message}\n[line #{lineno}]"
      end
    end
  end
end
