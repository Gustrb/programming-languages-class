# No módulo OpenClosedPrincipleViolation podemos perceber uma clara violação do princípio Open/Closed
# Em que, cada tipo de pagamento precisa, obrigatoriamente, ser adicionado no código com um novo if e
# um método subsequente.
# Para resolver isso, podemos criar uma classe base Payment e subclasses para cada tipo de pagamento
# Assim, o código fica mais limpo e fácil de manter, pois cada tipo de pagamento tem sua própria lógica
# E, se precisarmos adicionar um novo tipo de pagamento, basta criar uma nova classe que herda de Payment
# E implementar o método process
module OpenClosedPrincipleViolation
  class PaymentProcessor
    def process_payment(payment)
      if payment.type == 'credit_card'
        process_credit_card(payment)
      elsif payment.type == 'paypal'
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

  class Payment
    attr_reader :type, :amount
    def initialize(type, amount)
      @type = type
      @amount = amount
    end
  end
end

module OpenClosedPrinciple
  class PaymentProcessor
    def process_payment(payment)
      payment.process
    end
  end

  class CreditCardPayment
    attr_reader :amount

    def initialize(amount)
      @amount = amount
    end

    def process
      # Logic to process credit card payment
      puts "Processing credit card payment of #{@amount}"
    end
  end

  class PayPalPayment
    attr_reader :amount
    def initialize(amount)
      @amount = amount
    end

    def process
      # Logic to process PayPal payment
      puts "Processing PayPal payment of #{@amount}"
    end
  end
end
