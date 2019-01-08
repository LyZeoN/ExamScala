package com.fr.upem.partiel
import com.fr.upem.partiel.Part2.Categorie._

// Part2 (10pts)
/**
  *
  * The goal is to create a system that allows users to handle their personal finances.
  * Each user has an account with his UNIQUELY IDENTIFIED transactions.
  * Each transaction can be categorized with categories such as: Salary, Purchase, Withdrawal, Checks (deposits and payments) etc.
  *
  */
object Part2 {

  // 2.1 Modelling.
  // Create a model for the user's bank account
  final case class Account(id:Int,user:String, transaction :List[Transaction],mapTrans :Map[Int,Categorie])
  object Account{

    // 2.2 Create api
    // Create an api that allows for:
    // - Adding a transaction to a bank account
    // - Adding a transaction to a bank account with it's category
    // - Categorizing or recategorizing an existing transaction
    //
    // help: The bank account must save, for each transaction id, the transaction and it's eventual category
    // This could be achieved through a structure of this kind:
    // BankAccount(transactions: Map[TransactionId, CategorizedTransaction])

    def addTrans(a:Account,t:Transaction): Account ={
      a.copy(a.id,a.user,a.transaction.:+(t))
    }

    def addTrans(a:Account,t:Transaction,c:Categorie):Account = {
      a.copy(a.id,a.user,a.transaction.:+(t),a.mapTrans+(t.id -> c))
    }

    def reCatTrans(a:Account,id:Int,c: Categorie) : Account = {
      val newm = a.mapTrans + (id -> c)
      a.copy(mapTrans = newm)
    }
  }
  // Create a model for a transaction (has an amount and a date)
  final case class Transaction(id:Int,amount:Int,date :String)
  // Create a model for the following categories [salary, purchase, check deposit, check payment, withdrawal]
  sealed trait Categorie
  object Categorie{
    case object Salary extends Categorie
    case object Purchase extends Categorie
    case object Withdrawal extends Categorie
    case object ChecksD extends Categorie
    case object ChecksP extends Categorie
  }

  // 2.3 Use the api that you just created.
  // - Create an empty account
  // - Add a transaction with id 1 of amount -13 (and any date)
  // - Add a transaction with id 2 of amount -50 (and any date)
  // - Add a check payment with id 3 of amount 650 (and any date)
  // - Categorize the second transaction (id "2") as a withdrawal
  // - (Re)categorize the third transaction (id "3") as check deposit
  //
  // help: After the above operations the bank account should hold:
  // TransactionId(1) -> (Transaction(1, -13, date), None)
  // TransactionId(2) -> (Transaction(2, -50, date), Some(Withdrawal))
  // TransactionId(3) -> (Transaction(3, 650, date), Some(CheckDeposit)

  def main (args: Array[String]){
    val account = new Account(1,"empty",0, List.empty,Map.empty)
    val account2 = Account.addTrans(account,new Transaction(1,-13,"date"))
    val account3 = Account.addTrans(account2,new Transaction(2,-50,"date"))
    val account4 = Account.addTrans(account3,new Transaction(3,-650,"date"),ChecksP)
    val recat = Account.reCatTrans(account4,2,Withdrawal)
    val recat2 = Account.reCatTrans(recat,3,ChecksD)
  }

  // 2.4 CSV Export
  // Users want to be able to export their accounts in CSV (Comma-Separated Values) format.
  // A line is structured as follows: Id, Type, Amount, Date
  // Allow exporting a bank account as a CSV (no need to write a file, just write a String).
  // Amounts do not need to be formatted, write dates in any valid format (timestamp, ISO-8601 ...)
  //
  // Example output:
  // 1,check-deposit,300,1546784990415
  // 2,purchase,-24,1546698590604
  // 3,salary,3500,1546612190770
  // 4,,24,1546612190770

  object Export {

    def export(a: Account): Boolean ={

    }
  }

  // 2.5 CSV Import
  // Users want to be able to import transactions from a CSV.
  // Write code to parse and validate csv files
  // Validation: The input data should be validated
  //
  // Example valid input
  // 1,check-deposit,300,1546784990415
  // 2,purchase,-24,1546698590604
  // 3,salary,3500,1546612190770
  // 4,,24,1546612190770
  //
  // Example invalid input
  // 1,invalid type,invalid amount,invalid date

  // 2.6 Extend the api data analysis features
  // It should allow for:
  // - Sum all incomes (salaries, check deposits, uncategorized positive transactions)
  // - List all check (deposit and payment) operations
  // - Compute the account balance


}
