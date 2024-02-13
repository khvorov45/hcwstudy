const main = () => {
	const sites = [
		{
			short: "melbourne",
			long: "Alfred Hospital, Melbourne, VIC",
			link: "https://www.alfredhealth.org.au",
			investigator: "Prof Allen Cheng",
			coordinator: {
				name: "Clinical Research Infectious Diseases Unit",
				email: "clinresearch@alfred.org.au",
				phone: "0390 766 908",
			},
		},
		{
			short: "adelaide",
			long: "Women's and Children's Hospital, Adelaide, SA",
			link: "https://www.wch.sa.gov.au",
			investigator: "Prof Helen Marshall",
			coordinator: {
				name: "Kate Riley",
				email: "virtu@adelaide.edu.au",
				phone: "08 8161 6328",
			},
		},
		{
			short: "brisbane",
			long: "Queensland Children's Hospital, Brisbane, QLD",
			link: "https://www.childrens.health.qld.gov.au/qch",
			investigator: "A/Prof Julia Clark",
			coordinator: {
				name: "HealthCare Worker Flu Project Research Staff",
				email: "chq_idhcwflu@health.qld.gov.au",
				phone: "0429 206 919",
			},
		},
		{
			short: "newcastle",
			long: "John Hunter Hospital, New Lambton Heights, NSW",
			link: "http://www.hnehealth.nsw.gov.au/john-hunter-hospital/Pages/Home.aspx",
			investigator: "Prof Peter Wark",
			coordinator: {
				name: "Catherine Delahunty",
				email: "catherine.delahunty@newcastle.edu.au",
				phone: "0428 510 786",
			},
		},
		{
			short: "perth",
			long: "Perth Children's Hospital, Nedlands, WA",
			link: "https://pch.health.wa.gov.au",
			investigator: "A/Prof Christopher Blyth",
			coordinator: {
				name: "PCH HCW Flu Vaccine Study Team",
				email: "HCW.FluStudy@telethonkids.org.au",
				phone: "0476 302 022",
			},
		},
		{
			short: "sydney",
			long: "Children's Hospital at Westmead, NSW",
			link: "https://www.schn.health.nsw.gov.au/hospitals/chw",
			investigator: "Prof Kristine Macartney",
			coordinator: {
				name: "Ajay Jadhav",
				email: "SCHN-NCIRS-Research@health.nsw.gov.au",
				phone: "0429 849 440",
			},
		},
	]

	const decoder = new TextDecoder()
	const readFile = (path: string) => decoder.decode(Deno.readFileSync(path))

	const encoder = new TextEncoder()
	const template = readFile("template.html")
	const writePage = (name: string, contents: string) => {
		const page = template.replace("$CONTENT", () => contents)
		Deno.writeFileSync(name, encoder.encode(page))
	}

	{
		let home = readFile("home.html")

		{
			let participatingHospitals = ""
			for (const site of sites) {
				participatingHospitals += `<li><a href=${site.link}>${site.long}</a></li>\n`
			}
			home = home.replace("$PARTICIPATING_HOSPITALS", () => participatingHospitals)
		}

		{
			let participatingSites = ""
			for (const site of sites) {
				participatingSites += `<li>
				<a href=${site.link}>${site.long}</a>
				<ul><li>${site.investigator}</li></ul>
			</li>`
			}
			home = home.replace("$PARTICIPATING_SITES", () => participatingSites)
		}

		{
			let studyCoords = ""
			for (const site of sites) {
				studyCoords += `<li>
			<a href=${site.link}>${site.long}</a>
			<ul>
			  <li>${site.coordinator.name}</li>
			  <ul>
				<li>Email: <a href="mailto:${site.coordinator.email}">${site.coordinator.email}</a></li>
				<li>Phone: ${site.coordinator.phone}</li>
			  </ul>
			</ul></li>`
			}
			home = home.replace("$STUDY_COORDINATORS", () => studyCoords)
		}

		{
			let logos = `<div class="logos">`
			for (const site of sites) {
				logos += `<div class="logo-container ${site.short}-container">
				<img alt="logo-${site.short}" src="logo-${site.short}.png" class="logo-${site.short}"/>
			</div>`
			}
			logos += "</div>"
			home = home.replace("$LOGOS", () => logos)
		}

		writePage("index.html", home)
	}
}

main()
