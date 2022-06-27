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
require_relative "rlox/visitor/debug_visitor"
require_relative "rlox/visitor/evaluate_visitor"

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
      attr_reader :location

      def initialize(message, location = nil)
        super(message)
        @location = location
      end

      def detailed_message(source:, **)
        lineno = source[0..location.start].count("\n") + 1
        "[line #{lineno}] #{message}"
      end
    end

    # This is an error that occurs when a lox error is raised.
    class RuntimeError < Error
      attr_reader :location

      def initialize(message, location = nil)
        super(message)
        @location = location
      end

      def detailed_message(source:, **)
        lineno = source[0..location.start].count("\n") + 1
        "#{message}\n[line #{lineno}]"
      end
    end
  end
end
