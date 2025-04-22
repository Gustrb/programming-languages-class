# Neste módulo, a classe CoffeeMachine é uma classe base que define o método brew, add_water e brew_premium_coffee
# (como ruby não tem interfaces, usamos classes abstratas).
# As classes BasicCoffeeMachine e PremiumCoffeeMachine herdam de CoffeeMachine e implementam o método brew
# Porém a classe BasicCoffeeMachine não implementa o método brew_premium_coffee, o que causa uma violação do Princípio da Substituição de Liskov
# Isso significa que, se tivermos um código que espera um CoffeeMachine, não podemos passar um BasicCoffeeMachine
# porque ele não pode fazer tudo o que um CoffeeMachine pode fazer
module LiskovSubstitutionPrincipleViolation
  class CoffeeMachine
    def brew
      raise "not implemented"
    end

    def add_water(amount)
      raise "not implemented"
    end

    def brew_premium_coffee
      raise "not implemented"
    end
  end

  class BasicCoffeeMachine < CoffeeMachine
    def brew
      puts "Brewing basic coffee"
    end

    def add_water(amount)
      puts "Adding #{amount}ml of water to basic coffee machine"
    end

    def brew_premium_coffee
      raise "Basic coffee machine cannot brew premium coffee"
    end
  end

  class PremiumCoffeeMachine < CoffeeMachine
    def brew
      puts "Brewing premium coffee"
    end

    def add_water(amount)
      puts "Adding #{amount}ml of water to premium coffee machine"
    end

    def brew_premium_coffee
      puts "Brewing premium coffee in premium coffee machine"
    end
  end
end

# Neste módulo, a classe CoffeeMachine é uma classe base que define o método brew, add_water somente, fazendo com que
# as classes BasicCoffeeMachine e PremiumCoffeeMachine herdem de CoffeeMachine
# e implementem o método brew, mas não implementem o método brew_premium_coffee.
module LiskovSubstitutionPrinciple
  class CoffeeMachine
    def brew
      puts "Brewing coffee"
    end

    def add_water(amount)
      puts "Adding #{amount}ml of water"
    end
  end

  class BasicCoffeeMachine < CoffeeMachine
    def brew
      puts "Brewing basic coffee"
    end
  end

  class PremiumCoffeeMachine < CoffeeMachine
    def brew_premium_coffee
      puts "Brewing premium coffee"
    end
  end
end
