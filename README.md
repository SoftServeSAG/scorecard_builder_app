# Scorecard builder demo for AWS

This repository is a codebase for robust scorecard building based on the User's dataset

## Docker install ##

1. Get application source code:
```bash
git clone https://github.com/antonvais/scorecard_builder_app.git
cd scorecard_builder_app/
```

2. Build docker image:
```bash
docker build --rm --force-rm -t scorecard_builder_app .
```

3. Run docker image:
```bash
docker run --rm -p 3838:3838 scorecard_builder_app
```

4. Open your favorite browser and navigate to the `http://127.0.0.1:3838/scorecard_builder_app/`