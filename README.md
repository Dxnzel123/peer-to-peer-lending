### README.md

# Peer To Peer Lending  

**Description**  
The Peer To Peer Lending Smart Contract provides a decentralized platform for issuing and repaying loans. This system enables lenders to offer loans to borrowers with specific terms and ensures secure repayment with interest.  

---

## Features  
- **Offer Loans:**  
  Lenders can offer loans to specific borrowers with a specified amount, interest rate, and repayment deadline.  

- **Repay Loans:**  
  Borrowers can repay loans, including interest, before the deadline to clear their debt.  

- **Deadline Validation:**  
  Loans must be repaid before the agreed-upon deadline, ensuring accountability.  

- **Interest Calculation:**  
  The repayment amount includes the loan principal and interest.  

---

## Functions  

### Public Functions  

#### `offer-loan`  
**Parameters:**  
- `borrower (principal)`: The borrower's principal address.  
- `amount (uint)`: The loan amount.  
- `interest-rate (uint)`: The interest rate as a percentage.  
- `deadline (uint)`: The block height by which the loan must be repaid.  

**Behavior:**  
- Ensures that no existing loan has already been offered to the specified borrower.  
- Records the loan details, including the lender's principal address.  

**Returns:**  
- `ok "Loan offered successfully"`: If the loan is successfully offered.  
- `err "Loan already offered to this borrower"`: If a loan has already been offered to the borrower.  

---

#### `repay-loan`  
**Parameters:**  
- `borrower (principal)`: The borrower's principal address.  
- `amount (uint)`: The repayment amount provided by the borrower.  

**Behavior:**  
- Validates the existence of the loan.  
- Checks if the repayment amount covers the total repayable amount (principal + interest).  
- Ensures the loan is repaid before the deadline.  
- Removes the loan record upon successful repayment.  

**Returns:**  
- `ok "Loan repaid successfully"`: If the loan is fully repaid.  
- `err "No loan found for this borrower."`: If no loan exists for the borrower.  
- `err "Loan repayment deadline has passed."`: If the repayment deadline has passed.  
- `err "Insufficient repayment amount."`: If the repayment amount is inadequate.  

---

## Unit Tests  

Unit tests for the contract have been implemented to validate all scenarios, ensuring correctness and reliability.  

### Test Cases  

#### Loan Offering  
1. **Successful Loan Offer:**  
   - A lender can offer a loan to a borrower with valid terms.  

2. **Duplicate Loan Offer Prevention:**  
   - A lender cannot offer multiple loans to the same borrower.  

#### Loan Repayment  
1. **Successful Loan Repayment:**  
   - A borrower can repay the loan amount plus interest before the deadline.  

2. **No Loan Exists:**  
   - Repayment fails if no loan exists for the specified borrower.  

3. **Deadline Exceeded:**  
   - Repayment fails if the repayment is attempted after the deadline.  

4. **Insufficient Repayment Amount:**  
   - Repayment fails if the provided amount does not cover the total repayable amount.  

---

## Deployment  

1. Deploy the contract to your desired blockchain network.  
2. Test the contract thoroughly using the provided unit tests.  

---

## Example Usage  

### Offer a Loan  
```clarity
(offer-loan borrower-principal 1000 5 block-height + 10)
```  

### Repay a Loan  
```clarity
(repay-loan borrower-principal 1050)
```  

---

## Contributing  
Contributions are welcome! Fork the repository, implement your changes, and submit a pull request.  

---

## License  
This project is open-source under the MIT License.  
