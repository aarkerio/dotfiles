# === EDITOR ===
Pry.editor = 'enw'

# === PROMPT ===
 Pry.config.prompt = Pry::Prompt.new(
  'custom',
  'my custom prompt',
  [proc { |obj, nest_level, _| "#{obj}:#{nest_level}> " }]
)

# === COLORS ===
unless ENV['PRY_BW']
  Pry.color = true
  Pry.config.theme = 'railscasts'
  Pry::Prompt = PryRails::RAILS_PROMPT if defined?(PryRails::RAILS_PROMPT)
  Pry::Prompt ||= Pry.prompt
end

# === HISTORY ===
Pry.config.history_save = true

Pry::Commands.command /^$/, 'repeat last command' do
  pry_instance.run_command Pry.history.to_a.last
end

# == Pry-Nav - Using pry as a debugger ==
Pry.commands.alias_command 'c', 'continue' rescue nil
Pry.commands.alias_command 's', 'step' rescue nil
Pry.commands.alias_command 'n', 'next' rescue nil
Pry.commands.alias_command 'f', 'finish' rescue nil
Pry.commands.alias_command 'l', 'whereami' rescue nil
Pry.commands.alias_command 'webh', 'ShopifyAPI::Webhook.all' rescue nil

# === Listing config ===
# Better colors - by default the headings for methods are too
# similar to method name colors leading to a "soup"
# These colors are optimized for use with Solarized scheme
# for your terminal
Pry.config.ls.separator = "\n" # new lines between methods
Pry.config.ls.heading_color = :magenta
Pry.config.ls.public_method_color = :green
Pry.config.ls.protected_method_color = :yellow
Pry.config.ls.private_method_color = :bright_black

# == PLUGINS ===
# awesome_print gem: great syntax colorized printing
# look at ~/.aprc for more settings for awesome_print
begin
  require 'awesome_print'
  # The following line enables awesome_print for all pry output,
  # and it also enables paging
  Pry.config.print = proc {|output, value| Pry::Helpers::BaseHelpers.stagger_output("=> #{value.ai}", output)}

  # If you want awesome_print without automatic pagination, use the line below
  module AwesomePrint
    Formatter.prepend(Module.new do
      def awesome_self(object, type)
        if type == :string && @options[:string_limit] && object.inspect.to_s.length > @options[:string_limit]
          colorize(object.inspect.to_s[0..@options[:string_limit]] + "...", type)
        else
          super(object, type)
        end
      end
    end)
  end

  AwesomePrint.defaults = {
    string_limit: 80,
    indent: 2,
    multiline: true
  }
  AwesomePrint.pry!
rescue LoadError => err
  puts "gem install awesome_print  # <-- highly recommended"
end

# === CUSTOM COMMANDS ===
default_command_set = Pry::CommandSet.new do
  command 'sql', 'Send sql over AR.' do |query|
    if ENV['RAILS_ENV'] || defined?(Rails)
      pp ActiveRecord::Base.connection.select_all(query)
    else
      pp 'No rails env defined'
    end
  end
end

Pry.config.commands.import default_command_set

# === CONVENIENCE METHODS ===
class Array
  def self.sample(n=10, &block)
    block_given? ? Array.new(n,&block) : Array.new(n) {|i| i+1}
  end
end

class Hash
  def self.sample(n=10)
    (97...97+n).map(&:chr).map(&:to_sym).zip(0...n).to_h
  end
end

# === COLOR CUSTOMIZATION ===
# Everything below this line is for customizing colors, you have to use the ugly
# color codes, but such is life.
CodeRay.scan('example', :ruby).term # just to load necessary files

TERM_TOKEN_COLORS = {
       symbol: '1;31' # will make symbols bolded and light red on my terminal
}

CUSTOM_COLORS = {
  constant: '1;34',       # Bold Midnight Blue #191970
#  class_variable: '1;34',
  symbol: '1;31' # will make symbols bolded and light red on my terminal
}

colors = if (CodeRay::Encoders::Terminal::TOKEN_COLORS rescue nil)
         # CodeRay 1.0.0
         CodeRay::Encoders::Terminal::TOKEN_COLORS
       else
         # CodeRay 0.9
         begin
           require 'coderay/encoders/term'
           CodeRay::Encoders::Term::TOKEN_COLORS
         rescue
           nil
         end
       end

if colors
  CUSTOM_COLORS.each_pair do |key, value|
   colors[key] = value
  end
end

