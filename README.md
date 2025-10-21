# 🎓 Skill-to-Job Smart Contract

## 📋 Overview

A Stacks smart contract that connects certified learning programs with conditional job offers. Students who complete certified programs can unlock and apply for blockchain-verified job opportunities.

## ✨ Features

- 🏫 **Learning Program Registration** - Providers can create certified programs
- 🎯 **Student Certification** - Authorities issue verified certifications
- 💼 **Job Offer Creation** - Employers post conditional offers requiring specific certifications
- 🤝 **Application System** - Students apply and employers respond to applications
- 📊 **Analytics** - Track program statistics and match scores

## 🚀 Quick Start

### Prerequisites
- Clarinet installed
- Stacks wallet

### Deploy Contract
```bash
clarinet deploy
```

## 📖 Usage Guide

### For Learning Providers 👨‍🏫

1. **Register as Provider**
```clarity
(contract-call? .skill-to-job-smart-contract register-provider "CodeAcademy Pro")
```

2. **Create Learning Program**
```clarity
(contract-call? .skill-to-job-smart-contract create-learning-program 
  "Full Stack Development" 
  'SP123...CERT-AUTHORITY 
  u1000 
  (list "javascript" "react" "nodejs"))
```

### For Certification Authorities 🏛️

1. **Register Authority** (Admin only)
```clarity
(contract-call? .skill-to-job-smart-contract register-certification-authority "Tech Institute")
```

2. **Certify Students**
```clarity
(contract-call? .skill-to-job-smart-contract certify-student 
  'SP456...STUDENT 
  u1 
  u85 
  0x1234...hash)
```

### For Employers 🏢

1. **Create Job Offer**
```clarity
(contract-call? .skill-to-job-smart-contract create-job-offer 
  u1 
  "Senior Developer" 
  u50000 
  u52560 
  u5 
  u2000)
```

2. **Approve/Reject Applications**
```clarity
(contract-call? .skill-to-job-smart-contract approve-application 'SP456...STUDENT u1)
(contract-call? .skill-to-job-smart-contract reject-application 'SP456...STUDENT u1 "Position filled")
```

### For Students 🎓

1. **Apply for Jobs**
```clarity
(contract-call? .skill-to-job-smart-contract apply-for-job u1)
```

2. **Check Certification Status**
```clarity
(contract-call? .skill-to-job-smart-contract get-student-certification 'SP456...STUDENT u1)
```

## 🔍 Read-Only Functions

- `get-program` - Get program details
- `get-student-certification` - Check certification status
- `get-job-offer` - View job offer details
- `get-application` - Check application status
- `is-student-certified` - Verify certification
- `calculate-match-score` - Get student-offer compatibility score
- `get-contract-info` - View contract statistics

## 🗂️ Data Structure

### Programs
- Name, provider, certification authority
- Duration, skill tags, status

### Certifications
- Completion date, grade, verification status
- Certification hash for authenticity

### Job Offers
- Required program, position details
- Salary, duration, position limits
- Expiration and status

### Applications
- Application date, status
- Employer responses

## 🔐 Security Features

- Role-based access control
- Certification authority verification
- Offer expiration handling
- Anti-spam measures

## 🏗️ Architecture

```
Learning Providers → Programs → Certification Authorities → Students
                                        ↓
                              Student Certifications
                                        ↓
                    Job Offers ← Employers ← Applications
```

## 📊 Error Codes

- `u1001` - Unauthorized access
- `u1002` - Resource not found  
- `u1003` - Resource already exists
- `u1004` - Invalid program
- `u1005` - Student not certified
- `u1006` - Offer expired
- `u1007` - Already claimed
- `u1008` - Invalid offer

## 🤝 Contributing

1. Fork the repository
2. Create feature branch
3. Run `clarinet check` before submitting
4. Submit pull request

## 📄 License

MIT License
