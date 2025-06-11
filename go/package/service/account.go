package service

import (
	"fmt"

	"github.com/Gustrb/package/db"
)

type AccountService struct {
	db *db.FileDB
}

func NewAccountService(db *db.FileDB) *AccountService {
	return &AccountService{
		db: db,
	}
}

func (s *AccountService) GetBalance() (int, error) {
	return s.db.LookupBalance()
}

func (s *AccountService) Deposit(amount int) error {
	balance, err := s.db.LookupBalance()
	if err != nil {
		return fmt.Errorf("failed to lookup balance: %w", err)
	}

	balance += amount

	if err := s.db.UpdateBalance(balance); err != nil {
		return fmt.Errorf("failed to update balance: %w", err)
	}

	return nil
}

func (s *AccountService) Withdraw(amount int) error {
	balance, err := s.db.LookupBalance()
	if err != nil {
		return fmt.Errorf("failed to lookup balance: %w", err)
	}

	balance -= amount

	if balance < 0 {
		return fmt.Errorf("insufficient balance")
	}

	if err := s.db.UpdateBalance(balance); err != nil {
		return fmt.Errorf("failed to update balance: %w", err)
	}

	return nil
}
