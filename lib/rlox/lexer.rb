# frozen_string_literal: true

module Lox
  # This class is responsible for converting a source string into a stream of
  # tokens. It has some extra functionality to provide missing tokens if
  # necessary.
  class Lexer
    # A list of strings that represent all of the keywords in the language.
    KEYWORDS = %w[and class else false fun for if nil or print return super this true var while]

    # A list of the operators in the language keyed by their string representation
    # where the value is the name of the token that represents the operator.
    OPERATORS = {
      "(" => :LEFT_PAREN, ")" => :RIGHT_PAREN, "{" => :LEFT_BRACE, "}" => :RIGHT_BRACE, "," => :COMMA,
      "." => :DOT, "-" => :MINUS, "+" => :PLUS, ";" => :SEMICOLON, "/" => :SLASH, "*" => :STAR,
      "!" => :BANG, "!=" => :BANG_EQUAL, "=" => :EQUAL, "==" => :EQUAL_EQUAL, ">" => :GREATER,
      ">=" => :GREATER_EQUAL, "<" => :LESS, "<=" => :LESS_EQUAL
    }

    attr_reader :tokens

    def initialize(source)
      @tokens = lex(source)
    end

    def consume(type)
      peeked = peek

      if peeked in { type: ^type }
        self.next
      else
        AST::Token.new(type: :MISSING, location: peeked.location, value: nil)
      end
    end

    def each(&)
      tokens.each(&)
    end

    def match(type)
      self.next if peek in { type: ^type }
    end

    def next
      tokens.next
    end

    def peek
      tokens.peek
    end

    private

    def lex(source)
      Enumerator.new do |enum|
        index = 0
  
        while index < source.length
          case source[index..]
          in /\A(\/\/[^\n]*|\s+)/
            # skip whitespace and comments
          in /\A([(){},\.\-+;*\/]|[!=><]=?)/
            enum << AST::Token.new(
              type: OPERATORS[$&],
              location: AST::Location.new(start: index, finish: index + $&.length),
              value: $&
            )
          in /\A"([^"]*)"?/
            finish = index + $&.length
            unless $&.end_with?("\"")
              raise Error::SyntaxError.new("Error: Unterminated string.", AST::Location.new(start: index, finish: finish))
            end

            enum << AST::Token.new(
              type: :STRING,
              location: AST::Location.new(start: index, finish: finish),
              value: $&[1...-1]
            )
          in /\A\d+(\.\d+)?/
            enum << AST::Token.new(
              type: :NUMBER,
              location: AST::Location.new(start: index, finish: index + $&.length),
              value: $&.to_d
            )
          in /\A[a-z_][A-Za-z0-9_]*/
            type = KEYWORDS.include?($&) ? $&.upcase.to_sym : :IDENTIFIER
            enum << AST::Token.new(
              type: type,
              location: AST::Location.new(start: index, finish: index + $&.length),
              value: $&
            )
          end
  
          index += $&.length
        end
  
        enum << AST::Token.new(
          type: :EOF,
          location: AST::Location.new(start: index, finish: index),
          value: nil
        )
      end
    end
  end
end
