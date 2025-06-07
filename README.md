# 🆘 Reliefbit - Emergency Fund Disbursement System

## 📋 Overview

Reliefbit is a decentralized emergency fund disbursement system built on the Stacks blockchain using Clarity smart contracts. It enables transparent and efficient distribution of aid to verified victims during emergencies and disasters.

## ✨ Features

- 🏗️ **Emergency Creation**: Create and manage emergency fund campaigns
- 💰 **Fund Collection**: Accept STX donations for emergency relief
- ✅ **Victim Verification**: Verify and approve victims for aid disbursement  
- 🎯 **Automated Disbursement**: Release funds directly to verified victims
- 📊 **Transparent Tracking**: Monitor fund allocation and disbursement status
- 🔒 **Secure Management**: Owner-controlled verification and disbursement process

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone the repository
2. Navigate to the project directory
3. Deploy the contract using Clarinet

```bash
clarinet deploy
```

## 📖 Usage

### 🏗️ Creating an Emergency

Only the contract owner can create emergencies:

```clarity
(contract-call? .Reliefbit create-emergency "Earthquake Relief 2024" "Emergency fund for earthquake victims in affected areas")
```

### 💸 Funding an Emergency

Anyone can contribute STX to an emergency fund:

```clarity
(contract-call? .Reliefbit fund-emergency u1 u1000000) ;; Fund emergency ID 1 with 1 STX
```

### ✅ Verifying Victims

Contract owner verifies victims and sets their aid amount:

```clarity
(contract-call? .Reliefbit verify-victim 'SP1234...VICTIM emergency-id u500000) ;; 0.5 STX aid
```

### 🎯 Disbursing Aid

Contract owner releases funds to verified victims:

```clarity
(contract-call? .Reliefbit disburse-aid victim-id)
```

## 🔍 Read-Only Functions

- `get-emergency`: Get emergency details by ID
- `get-victim`: Get victim verification details
- `get-victim-by-address`: Check victim status by address
- `get-emergency-stats`: View emergency statistics
- `get-contract-balance`: Check total contract balance
- `is-victim-verified`: Verify if address is approved for aid

## 🛡️ Security Features

- Owner-only emergency creation and victim verification
- Duplicate verification prevention
- Fund availability checks before disbursement
- Emergency status management (active/inactive)
- Transparent fund tracking

## 📊 Contract Statistics

Track important metrics:
- Total funds collected per emergency
- Number of verified victims
- Amount disbursed vs remaining funds
- Emergency status and activity

## 🔧 Error Codes

- `u100`: Unauthorized access
- `u101`: Insufficient funds
- `u102`: Invalid amount
- `u103`: Already verified
- `u104`: Not verified
- `u105`: Already disbursed
- `u106`: Invalid emergency
- `u107`: Emergency inactive
- `u108`: Verification expired

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## 📄 License

This project is open source and available under the MIT License.

## 🆘 Support

For support and questions, please open an issue in the repository.


