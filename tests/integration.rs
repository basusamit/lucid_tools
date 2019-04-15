extern crate assert_cli;

#[cfg(test)]
mod integration {
    use assert_cli;
    use std::fs;


    #[test]
    fn parse_test() {
        let paths = fs::read_dir("./test/alchitry-labs").unwrap();
        for path in paths {
            assert_cli::Assert::main_binary()
                .with_args(&[path.unwrap().path().to_str().unwrap(),"junk.luc"])
                .unwrap();
        }
    }
}