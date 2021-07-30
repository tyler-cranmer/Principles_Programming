package edu.colorado.csci3155.project1

import org.scalatest.FunSuite

class StackMachineTest extends FunSuite {

    def testValueOfIdent(env: Map[String, Double], x: String, f: Double) = {
        assert( env contains x)
        assert( math.abs(env(x) - f) <= 1E-05)
    }

    test("PushI test") {
        val env = Map[String,Double] ()
        val stack = List[Double] ()
        val ins = PushI(2.0)
        val f = StackMachineEmulator.emulateSingleInstruction(stack, env, ins)
        assert(f == (List(2.0), env))
        print("stack : ", stack)
        print("\n",f)

    }
    test("StoreI test") {
        val _env: Map[String,Double] = Map("x"  -> 5.0, "y" -> 6.0, "im here" -> 10)
        val _stack:  List[Double] = List(55.0, 66.0)
        val _ins = StoreI("im here")
        val f = StackMachineEmulator.emulateSingleInstruction(_stack, _env, _ins)
        assert(f == (List(10.0 , 55.0, 66.0), _env))
        print(f)

    }

    test("StoreI test2 exception") {
        val _env: Map[String,Double] = Map("x"  -> 5.0, "y" -> 6.0, "im here" -> 10)
        val _stack:  List[Double] = List(55.0, 66.0)
        val _ins = StoreI("im here not")
        val f = intercept[Exception] { StackMachineEmulator.emulateSingleInstruction(_stack, _env, _ins)}
        assert(f.getMessage === "Value has not been defined")

    }
    test("PopI test") {
        val _env = Map[String,Double] ()
        val _stack:  List[Double] = List(4.0, 5.0, 6.0)
        val _ins = PopI
        val f = StackMachineEmulator.emulateSingleInstruction(_stack, _env, _ins)
        assert(f == (List(5.0,6.0), _env))
        println(_stack)
        print(f)
    }

    test("LoadI test") {
        val _env: Map[String,Double] = Map("x"  -> 5.0, "y" -> 6.0)
        val _stack:  List[Double] = List(10.0, 72.0, 3.0)
        val f = StackMachineEmulator.emulateSingleInstruction(_stack, _env, LoadI("result"))
        assert(f ==
          (List(72.0, 3.0), Map("x"  -> 5.0, "y" -> 6.0, "result" -> 10.0)))
    }

    test("AddI test") {
        val _env: Map[String,Double] = Map()
        val _stack:  List[Double] = List(10.0, 72.0, 3.0)
        val f = StackMachineEmulator.emulateSingleInstruction(_stack, _env, AddI)
        assert(f == (List(82.0, 3.0),_env))
        print(f)
    }

    test("SubI test") {
        val _env: Map[String,Double] = Map()
        val _stack:  List[Double] = List(10.0, 72.0, 3.0)
        val f = StackMachineEmulator.emulateSingleInstruction(_stack, _env, SubI)
        assert(f == (List(62.0, 3.0),_env))
        print(f)
    }

    test("MultI test") {
        val _env: Map[String,Double] = Map()
        val _stack:  List[Double] = List(10.0, 72.0, 3.0)
        val f = StackMachineEmulator.emulateSingleInstruction(_stack, _env, MultI)
        assert(f == (List(720.0, 3.0),_env))
        print(f)
    }

    test("DivI test") {
        val _env: Map[String,Double] = Map()
        val _stack:  List[Double] = List(10.0, 70.0, 3.0)
        val f = StackMachineEmulator.emulateSingleInstruction(_stack, _env, DivI)
        assert(f == (List(7.0, 3.0),_env))
        print(f)
    }

    test("ExpI test") {
        val _env: Map[String,Double] = Map()
        val _stack:  List[Double] = List(10.0, 70.0, 3.0)
        val f = StackMachineEmulator.emulateSingleInstruction(_stack, _env, ExpI)
        assert(f == (List(22026.465794806703, 70, 3.0),_env))
        print(f)
    }

    test("stack machine test 1") {
        val lst1 = List(PushI(2.5), PushI(3.5), AddI, LoadI("result"))
        val f = StackMachineEmulator.emulateStackMachine(lst1)
        assert(f("result") == 6.0)
    }

    test("stack machine test 2") {
        val lst1 = List(PushI(2.5), PushI(3.5), AddI, ExpI, LogI, LoadI("result"))
        val f = StackMachineEmulator.emulateStackMachine(lst1)
        assert(math.abs(f("result") - 6.0) <= 1e-05)
    }

    test("stack machine test 3") {
        val lst1 = List(PushI(3.5), PushI(2.5), PushI(4.5), PushI(5.2), AddI, LoadI("x"), LoadI("y"), LoadI("z"), StoreI("y"), LoadI("w"))
        val fenv = StackMachineEmulator.emulateStackMachine(lst1)
        assert(fenv contains "x")
        assert(fenv contains "y")
        assert(fenv contains "z")
        assert( math.abs(fenv("x") - 9.7 ) <= 1e-05 )
        assert( math.abs(fenv("y") - 2.5 ) <= 1e-05 )
        assert( math.abs(fenv("z") - 3.5 ) <= 1e-05 )
    }

    test("stack machine test 4") {
        val lst4 = List(PushI(3.5), PushI(2.5), PushI(4.5), PushI(5.2),  LoadI("x"), LoadI("y"), LoadI("z"), LoadI("w"),
                         StoreI("y"), StoreI("w"), AddI, LoadI("res1"), StoreI("x"), StoreI("z"), MultI, LoadI("res2"))
        val fenv = StackMachineEmulator.emulateStackMachine(lst4)
        assert(fenv contains "x")
        assert(fenv contains "y")
        assert(fenv contains "z")
        assert(fenv contains "w")
        assert(fenv contains "res1")
        assert(fenv contains "res2")
        testValueOfIdent(fenv, "x", 5.2)
        testValueOfIdent(fenv, "y", 4.5)
        testValueOfIdent(fenv, "z", 2.5)
        testValueOfIdent(fenv, "w", 3.5)
        testValueOfIdent(fenv, "res1", 8.0)
        testValueOfIdent(fenv, "res2", 5.2*2.5)
    }

    test("stack machine test 5") {
        val lst1 = List(PushI(1.5), PushI(2.4), AddI, PushI(2.5), PushI(2.5), MultI, SubI)
        val lst2 = List(PushI(1.0), PushI(2.5), AddI)
        val lst3 = lst1 ++ List(ExpI) ++ lst2 ++ List(ExpI) ++ List(AddI) ++ List(LogI) ++ List(LoadI("result"))
        val f = StackMachineEmulator.emulateStackMachine(lst3)
        assert(math.abs(f("result") - 3.50287) <= 1E-04)
    }


}
