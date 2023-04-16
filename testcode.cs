using System;

namespace BankSystem
{
    class BankAccount
    {
        public string accountNumber;
        private string accountHolder;
        private double balance;

        public BankAccount(string accountNumber, string accountHolder, double balance)
        {
            this.accountNumber = accountNumber;
            this.accountHolder = accountHolder;
            this.balance = balance;
        }

        public void Deposit(double amount)
        {
            if (amount > 0)
            {
                balance += amount;
                Console.WriteLine($"Deposit of {amount:C} successful. New balance: {balance:C}");
            }
            else
            {
                Console.WriteLine("Invalid deposit amount.");
            }
        }

        public void Withdraw(double amount)
        {
            if (amount > 0 && amount <= balance)
            {
                balance -= amount;
                Console.WriteLine($"Withdrawal of {amount:C} successful. New balance: {balance:C}");
            }
            else
            {
                Console.WriteLine("Invalid withdrawal amount or insufficient funds.");
            }
        }

        public void DisplayAccountInfo()
        {
            Console.WriteLine($"Account number: {accountNumber}");
            Console.WriteLine($"Account holder: {accountHolder}");
            Console.WriteLine($"Current balance: {balance:C}");
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            BankAccount account = new BankAccount("1234567890", "John Smith", 1000.0);
            account.DisplayAccountInfo();
            account.Deposit(500.0);
            account.Withdraw(200.0);
            account.Withdraw(1500.0);
        }
    }

    
}
