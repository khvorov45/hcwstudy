const execCmd = async (cmd: string): Promise<void> => {
	const args = cmd.split(" ")
	const process = Deno.run({ cmd: args })
	const status = await process.status()
	if (!status.success) Deno.exit(1)
}

const main = async (): Promise<void> => {
	await execCmd("deno bundle frontend.ts bundle.js")
	if (Deno.args[0] === "release") {
		const minifiedHtmlPath = "index.html"
		await execCmd(
			`html-minifier-terser --collapse-whitespace --minify-css true frontend.html -o ${minifiedHtmlPath}`
		)
		const minifiedScriptPath = "bundle.min.js"
		await execCmd(`terser bundle.js -o ${minifiedScriptPath} --compress toplevel=true --mangle toplevel=true`)
		const decoder = new TextDecoder()
		const minifiedScript = decoder.decode(Deno.readFileSync(minifiedScriptPath))
		const minifiedHtml = decoder.decode(Deno.readFileSync(minifiedHtmlPath))
		const minifiedHtmlWithScript = minifiedHtml.replace(
			`<script type="text/javascript" src="bundle.js"></script>`,
			// NOTE(sen) If this is not a function, special characters in the replacement string
			// will be treated special
			() => `<script type="text/javascript">${minifiedScript}</script>`
		)
		const encoder = new TextEncoder()
		Deno.writeFileSync(minifiedHtmlPath, encoder.encode(minifiedHtmlWithScript))
	}
}

await main()
