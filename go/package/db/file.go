package db

import (
	"fmt"
	"os"
	"strconv"
	"sync"
)

// The database is a file in the filesystem that contains just a number.
type FileDB struct {
	filePath string
	mu       *sync.Mutex
}

func NewFileDB(filepath string) (*FileDB, error) {
	if _, err := os.Create(filepath); err != nil {
		return nil, fmt.Errorf("failed to create file: %w", err)
	}

	return &FileDB{
		filePath: filepath,
		mu:       &sync.Mutex{},
	}, nil
}

func (db *FileDB) LookupBalance() (int, error) {
	db.mu.Lock()
	defer db.mu.Unlock()

	bytes, err := os.ReadFile(db.filePath)
	if err != nil {
		return 0, err
	}

	if len(bytes) == 0 {
		return 0, nil
	}

	balance, err := strconv.Atoi(string(bytes))
	if err != nil {
		return 0, err
	}

	return balance, nil
}

func (db *FileDB) UpdateBalance(balance int) error {
	db.mu.Lock()
	defer db.mu.Unlock()

	bytes := []byte(strconv.Itoa(balance))

	return os.WriteFile(db.filePath, bytes, 0644)
}
