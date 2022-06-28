# frozen_string_literal: true

require "bigdecimal"
require "bigdecimal/util"

require_relative "lox/ast"
require_relative "lox/error"
require_relative "lox/lexer"
require_relative "lox/parser"

require_relative "lox/type"
require_relative "lox/type/false"
require_relative "lox/type/function"
require_relative "lox/type/nil"
require_relative "lox/type/number"
require_relative "lox/type/string"
require_relative "lox/type/true"

require_relative "lox/visitor"
require_relative "lox/visitor/interpreter"
require_relative "lox/visitor/pretty_printer"
require_relative "lox/visitor/resolver"

# The top-level class that provides all of the functionality of the language.
module Lox
  class << self
    # This is the main entry point for the lox interpreter. It parses the given
    # source into a tree and then walks it to interpret it. The return value of
    # this function is the exit code of the program.
    def interpret(source)
      parser = Lox::Parser.new
      program = parser.parse(source)
      handle_syntax_errors(source, parser.errors)

      interpreter = Lox::Visitor::Interpreter.new
      program.accept(Lox::Visitor::Resolver.new(interpreter))
      handle_syntax_errors(source, interpreter.errors)

      program.accept(interpreter)
      0
    rescue Lox::Error::SyntaxError => error
      handle_syntax_errors(source, [error])
    rescue Lox::Error::RuntimeError => error
      warn(error.detailed_message(source: source))
      Lox::Error::RuntimeError::EXIT_CODE
    end

    private

    def handle_syntax_errors(source, errors)
      return if errors.empty?

      errors.each { |error| warn(error.detailed_message(source: source)) }
      exit Lox::Error::SyntaxError::EXIT_CODE
    end
  end
end
