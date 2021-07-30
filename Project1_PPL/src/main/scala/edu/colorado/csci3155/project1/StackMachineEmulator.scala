package edu.colorado.csci3155.project1


/* -- Here are all the instructions to be supported --*/
sealed trait StackMachineInstruction
case class LoadI(s: String) extends StackMachineInstruction
case class  StoreI(s: String) extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {



    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack
              a map from string to double precision numbers for the environment
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified environment that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */


    def emulateSingleInstruction(stack: List[Double],
                                 env: Map[String, Double],
                                 ins: StackMachineInstruction): (List[Double], Map[String, Double]) = {

        ins match {
            case LoadI(s: String) => (stack.tail, env + (s -> stack.head))
            case StoreI(s: String) => if (env.contains(s)) { ((env(s)::stack) ,env ) } else
                throw new Exception("Value has not been defined")
            case PushI(f: Double) => ((f::stack), env)
            case PopI => if (stack.length <= 0) {throw new Exception("Not enough values in the stack") } else (stack.tail, env)
            case AddI => if (stack.length <= 1) {throw new Exception("Not enough values in the stack") } else ((stack.head + stack.tail.head :: stack.tail.tail), env)
            case SubI => if (stack.length <= 1) {throw new Exception("Not enough values in the stack") } else {
                val v1 = stack.head
                val v2 = stack.tail.head
                ((v2 - v1 :: stack.tail.tail) , env)
            }
            case MultI => if (stack.length <= 1) {throw new Exception("Not enough values in the stack") } else {
                ((stack.head * stack.tail.head :: stack.tail.tail), env)
            }
            case DivI => if (stack.length <= 1) {throw new Exception("Not enough values in the stack") } else {
                val v1 = stack.head
                val v2 = stack.tail.head
                ((v2 / v1 :: stack.tail.tail) , env)
            }
            case ExpI => if (stack.length < 1) {throw new Exception("Not enough values in the stack") } else {
                (scala.math.pow(scala.math.E,stack.head):: stack.tail, env)
            }
            case LogI  => if (stack.length < 1) {throw new Exception("Not enough values in the stack") } else {
                (math.log(stack.head)::stack.tail, env)
            }
            case SinI  => if (stack.length < 1) {throw new Exception("Not enough values in the stack") } else {
                (math.sin(stack.head) :: stack.tail, env)
            }
            case CosI  => if (stack.length < 1) {throw new Exception("Not enough values in the stack")  } else {
                (math.cos(stack.head):: stack.tail, env)
            }
        }

    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be the final environment.

       Hint: accumulator for foldLeft must be a tuple (List[Double], Map[String,Double])
             initial value of this accumulator must be (Nil, Map.empty)
             You should use emulateSingleInstruction to update the accmulator.
             It will all fit nicely once you figure it out.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Map[String, Double] = {

      val result = instructionList.foldLeft((Nil: List[Double], Map.empty: Map[String, Double])) ((acc, elt) => emulateSingleInstruction(acc._1, acc._2, elt ))

        result._2
    }

}
