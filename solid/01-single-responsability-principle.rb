# Esse módulo é um exemplo de violação do principio da responsabilidade única
# O módulo SingleResponsabilityPrincipleViolation tem uma classe UserService que tem várias responsabilidades
# A classe UserService é responsável por criar usuários, validar e hashear senhas e enviar emails
# O ideal é separar essas responsabilidades em classes diferentes
module SingleResponsabilityPrincipleViolation
    class UserService
        def initialize
            @users = []
        end
    
        def create_user(email, password)
            if !valid_email?(email)
                raise "Invalid email format"
            end
            if !valid_password?(password)
                raise "Password must be at least 2 characters long"
            end

            password_digest = hash_password(password)
    
            user = {
                email: email,
                password: password_digest
            }
    
            @users << user
            send_welcome_email(email)
        end
    
        def hash_password(password)
            # Hashing logic
            return "hashed_#{password}"
        end
    
        def send_welcome_email(email)
            puts "Sending welcome email to #{email}"
        end

        private

        def valid_email?(email)
            return true
        end

        def valid_password?(password)
            return password.length >= 2
        end
    end    
end

# Aqui podemos ver uma refatoração do código acima, onde o módulo SingleResponsabilityPrinciple tem uma classe UserService que tem apenas uma responsabilidade
# A classe UserService é responsável por criar usuários, mas não tem as responsabilidades de validar e hashear senhas e enviar emails
# As responsabilidades de validar e hashear senhas e enviar emails foram separadas em classes diferentes
# O ideal é separar essas responsabilidades em classes diferentes
module SingleResponsabilityPrinciple
    class UserService
        def initialize(user_repository, email_service, password_service)
            @user_repository = user_repository
            @email_service = email_service
            @password_service = password_service
        end

        def create_user(email, password)
            raise "Invalid email format" if !@email_service.valid_email?(email)
            raise "Password must be at least 2 characters long" if !@password_service.valid_password?(password)

            password_digest = @password_service.hash_password(password)

            user = {
                email: email,
                password: password_digest
            }

            @user_repository.save(user)
            @email_service.send_welcome_email(email)
        end
    end

    class UserRepository
        def initialize
            @users = []
        end

        def save(user)
            @users << user
        end
    end

    class EmailService
        def send_welcome_email(email)
            puts "Sending welcome email to #{email}"
        end

        def valid_email?(email)
            return true
        end
    end

    class PasswordService
        def hash_password(password)
            return "hashed_#{password}"
        end

        def valid_password?(password)
            return password.length >= 2
        end
    end
end

s = SingleResponsabilityPrinciple::UserService.new(
    SingleResponsabilityPrinciple::UserRepository.new,
    SingleResponsabilityPrinciple::EmailService.new,
    SingleResponsabilityPrinciple::PasswordService.new
)

s.create_user("email", "12334")
