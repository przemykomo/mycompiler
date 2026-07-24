use std::path::Path;

use inkwell::{
    OptimizationLevel,
    context::Context,
    llvm_sys::target_machine::LLVMCreateTargetMachine,
    module::Module,
    targets::{
        FileType, InitializationConfig, Target, TargetMachine, TargetMachineOptions, TargetTriple,
    },
};

use crate::IRGen;

pub fn run<'a>(irgen: &'a IRGen<'a>, out_file: &String) {
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("tmp");

    let ret_type = context.i64_type();
    let fn_type = ret_type.fn_type(&[], false);
    let function = module.add_function("main", fn_type, None);

    let entry = context.append_basic_block(function, "entry");
    builder.position_at_end(entry);
    // let alloca = builder.build_alloca(context.i64_type(), "var_alloca").unwrap();
    // builder.build_store(alloca, ).unwrap();

    let const_int = context.i64_type().const_int(23, true);
    let const_intb = context.i64_type().const_int(4, true);
    let add_result = builder
        .build_int_add(const_int, const_intb, "tmpadd")
        .unwrap();

    builder.build_return(Some(&add_result)).unwrap();

    function.print_to_stderr();

    if !function.verify(true) {
        unsafe { function.delete() };
        panic!("function verify");
    }

    // let triple = TargetMachine::get_default_triple();
    // let target = Target::create_target_machine_from_options(&self, &triple, options);

    Target::initialize_x86(&InitializationConfig::default());

    let triple = TargetTriple::create("x86_64-pc-linux-gnu");
    let target = Target::from_triple(&triple).unwrap();
    let options = TargetMachineOptions::default()
        .set_cpu("x86-64")
        // .set_features("+avx2")
        .set_abi("sysv")
        .set_level(OptimizationLevel::None);

    let target_machine = target
        .create_target_machine_from_options(&triple, options)
        .unwrap();

    module.set_data_layout(&target_machine.get_target_data().get_data_layout());
    module.set_triple(&triple);

    target_machine
        .write_to_file(&module, FileType::Object, Path::new(out_file))
        .unwrap();
}
