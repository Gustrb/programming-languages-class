package main

import (
	"errors"
	"fmt"
	"log"
	"math/rand"
	"sync"

	"github.com/Gustrb/package/db"
	"github.com/Gustrb/package/service"
)

type AccountOperation string

const (
	AccountOperationDeposit  AccountOperation = "deposit"
	AccountOperationWithdraw AccountOperation = "withdraw"
	AccountOperationBalance  AccountOperation = "balance"
)

var validOperationTypes = []AccountOperation{
	AccountOperationDeposit,
	AccountOperationWithdraw,
}

type AccountOperationRequest struct {
	Operation AccountOperation
	Amount    int
}

func main() {
	operations := generateRandomAccountOperations(100)
	db, err := db.NewFileDB("balance.txt")
	if err != nil {
		log.Fatalf("failed to create db: %v", err)
	}

	wg := sync.WaitGroup{}
	errs := make([]error, 0, len(operations))
	service := service.NewAccountService(db)
	for _, operation := range operations {
		wg.Add(1)
		go func() {
			defer wg.Done()
			switch operation.Operation {
			case AccountOperationDeposit:
				err := service.Deposit(operation.Amount)
				if err != nil {
					errs = append(errs, err)
				}
			case AccountOperationWithdraw:
				err := service.Withdraw(operation.Amount)
				if err != nil {
					errs = append(errs, err)
				}
			case AccountOperationBalance:
				// do nothing
			}
		}()
	}

	wg.Wait()

	balance, err := service.GetBalance()
	if err != nil {
		log.Fatalf("failed to get balance: %v", err)
	}

	if err := errors.Join(errs...); err != nil {
		log.Printf("failed to perform operations: %v", err)
	}

	fmt.Println("balance at the end:", balance)
}

func generateRandomAccountOperations(n int) []AccountOperationRequest {
	operations := make([]AccountOperationRequest, n)

	for i := range n {
		operationType := validOperationTypes[rand.Intn(len(validOperationTypes))]

		operations[i] = AccountOperationRequest{
			Operation: operationType,
			Amount:    rand.Intn(1000),
		}
	}

	return operations
}
