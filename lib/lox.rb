# frozen_string_literal: true

require "bigdecimal"
require "bigdecimal/util"

require_relative "lox/ast"
require_relative "lox/error"
require_relative "lox/lexer"
require_relative "lox/parser"

require_relative "lox/type"
require_relative "lox/type/false"
require_relative "lox/type/nil"
require_relative "lox/type/number"
require_relative "lox/type/string"
require_relative "lox/type/true"

require_relative "lox/visitor"
require_relative "lox/visitor/interpreter"
require_relative "lox/visitor/pretty_printer"

# The top-level class that provides all of the functionality of the language.
module Lox
  # This is the main entry point for the lox interpreter. It parses the given
  # source into a tree and then walks it to interpret it. The return value of
  # this function is the exit code of the program.
  def self.interpret(source)
    parser = Lox::Parser.new
    program = parser.parse(source)

    if parser.errors.any?
      parser.errors.each { |error| warn(error.detailed_message(source: source)) }
      exit Lox::Error::SyntaxError::EXIT_CODE
    end

    program.accept(Lox::Visitor::Interpreter.new)
    0
  rescue Lox::Error::SyntaxError => error
    warn(error.detailed_message(source: source))
    Lox::Error::SyntaxError::EXIT_CODE
  rescue Lox::Error::RuntimeError => error
    warn(error.detailed_message(source: source))
    Lox::Error::RuntimeError::EXIT_CODE
  end
end
