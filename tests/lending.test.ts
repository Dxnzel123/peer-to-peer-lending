import { describe, it, beforeEach, expect } from 'vitest';

// Mock state and functions for the loan contract
let loans: Record<string, any>;
let blockHeight: number;

beforeEach(() => {
  // Initialize state before each test
  loans = {};
  blockHeight = 1000; // Simulated block height
});

// Mock contract functions
const offerLoan = (borrower: string, amount: number, interestRate: number, deadline: number) => {
  if (loans[borrower]) {
    throw new Error('Loan already offered to this borrower');
  }
  loans[borrower] = {
    amount,
    interestRate,
    deadline,
    lender: 'tx-sender',
    borrower,
  };
  return 'Loan offered successfully';
};

const repayLoan = (borrower: string, amount: number) => {
  const loan = loans[borrower];
  if (!loan) {
    throw new Error('No loan found for this borrower.');
  }
  const totalRepayable = loan.amount + (loan.amount * loan.interestRate) / 100;
  if (blockHeight > loan.deadline) {
    throw new Error('Loan repayment deadline has passed.');
  }
  if (amount < totalRepayable) {
    throw new Error('Insufficient repayment amount.');
  }
  delete loans[borrower];
  return 'Loan repaid successfully';
};

// Test Suite
describe('Loan Contract', () => {
  it('should offer a loan successfully', () => {
    const result = offerLoan('borrower1', 1000, 5, blockHeight + 10);
    expect(result).toBe('Loan offered successfully');
    expect(loans['borrower1']).toMatchObject({
      amount: 1000,
      interestRate: 5,
      deadline: blockHeight + 10,
      lender: 'tx-sender',
      borrower: 'borrower1',
    });
  });

  it('should not allow offering a loan to the same borrower twice', () => {
    offerLoan('borrower1', 1000, 5, blockHeight + 10);
    expect(() => offerLoan('borrower1', 2000, 5, blockHeight + 20)).toThrow(
      'Loan already offered to this borrower'
    );
  });

  it('should repay a loan successfully', () => {
    offerLoan('borrower1', 1000, 5, blockHeight + 10);
    const result = repayLoan('borrower1', 1050); // Total repayable = 1000 + (1000 * 5 / 100)
    expect(result).toBe('Loan repaid successfully');
    expect(loans['borrower1']).toBeUndefined(); // Loan should be removed after repayment
  });

  it('should not repay a loan if no loan exists', () => {
    expect(() => repayLoan('borrower1', 1000)).toThrow('No loan found for this borrower.');
  });

  it('should not repay a loan if the repayment deadline has passed', () => {
    offerLoan('borrower1', 1000, 5, blockHeight + 1);
    blockHeight += 2; // Simulate time passing
    expect(() => repayLoan('borrower1', 1050)).toThrow('Loan repayment deadline has passed.');
  });

  it('should not repay a loan with insufficient amount', () => {
    offerLoan('borrower1', 1000, 5, blockHeight + 10);
    expect(() => repayLoan('borrower1', 1000)).toThrow('Insufficient repayment amount.');
  });
});
