use crate::analysis::StaticAnalysisResults;
use crate::parse::Module;
use std::path::PathBuf;


struct GeneratedSourceFile {
    filepath: PathBuf,
    source: String
}


impl GeneratedSourceFile {

}


pub fn compile(root_path: &PathBuf, module: &Module, analysis: &StaticAnalysisResults) {
    /* Creates a GeneratedSourceFile to compile into */

    let mut generated_source = GeneratedSourceFile {
        filepath: root_path
                    .join("target")
                    .join("generated_c")
                    .join(module.module_name()),
        source: String::new()
    };

    generated_source.filepath.set_extension("c");
}
