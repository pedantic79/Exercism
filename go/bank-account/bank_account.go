package account

import (
	"sync"
)

type Account struct {
	open    bool
	balance int
	lock    sync.RWMutex
}

func Open(starting int) *Account {
	if starting < 0 {
		return nil
	}

	return &Account{
		balance: starting,
		open:    true,
	}
}

func (acct *Account) Balance() (int, bool) {
	acct.lock.RLock()
	defer acct.lock.RUnlock()

	if !acct.open {
		return 0, false
	}

	return acct.balance, true
}

func (acct *Account) Close() (int, bool) {
	acct.lock.Lock()
	defer acct.lock.Unlock()

	if !acct.open {
		return 0, false

	}
	acct.open = false
	return acct.balance, true
}

func (acct *Account) Deposit(amount int) (int, bool) {
	acct.lock.Lock()
	defer acct.lock.Unlock()

	if !acct.open || (amount < 0 && acct.balance+amount < 0) {
		return 0, false
	}
	acct.balance += amount
	return acct.balance, true
}
