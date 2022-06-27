# frozen_string_literal: true

require "bigdecimal"
require "bigdecimal/util"

require_relative "rlox/ast"
require_relative "rlox/lexer"
require_relative "rlox/parser"
require_relative "rlox/type"
require_relative "rlox/type/false"
require_relative "rlox/type/nil"
require_relative "rlox/type/number"
require_relative "rlox/type/string"
require_relative "rlox/type/true"
require_relative "rlox/visitor"
require_relative "rlox/visitor/interpreter"
require_relative "rlox/visitor/pretty_printer"

# The top-level class that provides all of the functionality of the language.
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

  def self.interpret(source)
    parser = Lox::Parser.new
    program = parser.parse(source)

    if parser.errors.any?
      parser.errors.each { |error| warn(error.detailed_message(source: source)) }
      exit Lox::Error::SyntaxError::EXIT_CODE
    end

    program.accept(Lox::Visitor::Interpreter.new)
  rescue Lox::Error::SyntaxError => error
    warn(error.detailed_message(source: source))
    exit Lox::Error::SyntaxError::EXIT_CODE
  rescue Lox::Error::RuntimeError => error
    warn(error.detailed_message(source: source))
    exit Lox::Error::RuntimeError::EXIT_CODE
  end
end
