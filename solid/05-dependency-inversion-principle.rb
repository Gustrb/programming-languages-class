# No módulo DependecyInversionPrincipleViolation podemos perceber uma clara violação do princípio Dependency Inversion
# Em que a classe PaymentProcessor depende diretamente de classes concretas (CreditCardPayment e PayPalPayment)
# E não de abstrações
# Para resolver isso, podemos criar uma interface PaymentGateway e classes concretas que implementam essa interface
# Assim, a classe PaymentProcessor depende de abstrações e não de classes concretas
# Isso torna o código mais flexível e fácil de manter, pois podemos adicionar novos gateways de pagamento
# sem precisar modificar a classe PaymentProcessor
module DependecyInversionPrincipleViolation
  class PaymentProcessor
    def process_payment(payment)
      case payment.type
      when "credit_card"
        process_credit_card(payment)
      when "paypal"
        process_paypal(payment)
      else
        raise "Unsupported payment type"
      end
    end

    private
    def process_credit_card(payment)
      # Logic to process credit card payment
      puts "Processing credit card payment of #{payment.amount}"
    end

    def process_paypal(payment)
      # Logic to process PayPal payment
      puts "Processing PayPal payment of #{payment.amount}"
    end
  end
end

module DependecyInversionPrinciple
  class PaymentProcessor
    def initialize(payment_gateway)
      @payment_gateway = payment_gateway
    end

    def process_payment(payment)
      @payment_gateway.process(payment)
    end
  end

  class CreditCardPayment
    attr_reader :amount

    def initialize(amount)
      @amount = amount
    end
  end

  class PayPalPayment
    attr_reader :amount

    def initialize(amount)
      @amount = amount
    end
  end

  class PayPalGateway
    def process(payment)
      puts "Processing PayPal payment of #{payment.amount}"
    end
  end

  class CreditCardGateway
    def process(payment)
      puts "Processing credit card payment of #{payment.amount}"
    end
  end
end