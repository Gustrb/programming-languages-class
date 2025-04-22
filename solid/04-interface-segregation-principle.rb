# No módulo InterfaceSegregationPrincipleViolation criamos uma interface Appliance que tem métodos
# que não são utilizados por todas as classes que a implementam
# Isso é uma violação do princípio Interface Segregation, pois as classes que implementam a interface
# não precisam de todos os métodos definidos na interface.
# Para resolver isso, podemos criar interfaces separadas para cada tipo de funcionalidade
module InterfaceSegregationPrincipleViolation
  class Appliance
    def turn_on; raise "not implemented"; end
    def turn_off; raise "not implemented"; end
    def brew; raise "not implemented"; end
    def clean_room; raise "not implemented"; end
    def play_music; raise "not implemented"; end
  end

  class SmartLight < Appliance
    def turn_on
      puts "Smart light turned on"
    end

    def turn_off
      puts "Smart light turned off"
    end

    def brew; end
    def clean_room; end
    def play_music; end
  end

  class SmartVacuum < Appliance
    def turn_on
      puts "Smart vacuum turned on"
    end

    def turn_off
      puts "Smart vacuum turned off"
    end

    def brew; end
    def play_music; end

    def clean_room
      puts "Smart vacuum cleaning the room"
    end
  end
end

module InterfaceSegregationPrinciple
  class Switchable
    def turn_on; raise "not implemented"; end
    def turn_off; raise "not implemented"; end
  end

  class Brewable
    def brew; raise "not implemented"; end
  end

  class Cleanable
    def clean_room; raise "not implemented"; end
  end

  class CleanableSwitchable < Switchable
    def clean_room; raise "not implemented"; end
  end

  class Playable
    def play_music; raise "not implemented"; end
  end

  class SmartLight < Switchable
    def turn_on
      puts "Smart light turned on"
    end

    def turn_off
      puts "Smart light turned off"
    end
  end

  class SmartVacuum < CleanableSwitchable
    def turn_off
      puts "Smart vacuum turned off"
    end
    def turn_on
      puts "Smart vacuum turned on"
    end
    def clean_room
      puts "Smart vacuum cleaning the room"
    end
  end
end
