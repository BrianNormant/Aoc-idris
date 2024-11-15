{
	description = "A very basic flake";

	inputs = {
		nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
		# idris2-jvm.url = "github:mmhelloworld/idris-jvm?ref=v0.7.0";
	};

	outputs = { self, nixpkgs }: let
		system = "x86_64-linux";
		pkgs = import nixpkgs { inherit system; };
		# idris2JVMPackages = idris2-jvm.packages.${system};
	in {
		devShells."${system}".default = pkgs.mkShell {
			packages = with pkgs; [
				# idris2JVMPackages.idris2
                idris2
				idris2Packages.pack
				gmp
				zsh
				chez
				rlwrap
			];
			shellHook = ''
				export SHELL=zsh
				exec zsh
				'';
		};
	};
}
