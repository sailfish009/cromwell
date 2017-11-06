import sbt._

object Dependencies {
  val akkaHttpV = "10.0.10"
  val akkaV = "2.5.4"
  val ammoniteOpsV = "1.0.1"
  val apacheHttpClientV = "4.5.3"
  val betterFilesV = "2.17.1"
  val catsEffectV = "0.4"
  val catsV = "1.0.0-MF"
  val circeV = "0.9.0-M1"
  val circeYamlV = "0.7.0-M1"
  val commonsCodecV = "1.10"
  val commonsIoV = "2.5"
  val commonsTextV = "1.1"
  val configsV = "0.4.4"
  val ficusV = "1.4.1"
  val fs2V = "0.9.7"
  val googleClientApiV = "1.22.0"
  val googleCloudComputeV = "0.26.0-alpha"
  val googleCloudNioV = "0.20.1-alpha"
  val googleCredentialsV = "0.8.0"
  val googleGenomicsServicesApiV = "v1alpha2-rev64-1.22.0"
  val googleOauth2V = "0.8.0"
  val grpcV = "1.5.0"
  val guavaV = "22.0"
  val hsqldbV = "2.3.4"
  val jacksonV = "2.8.9"
  val janinoV = "3.0.7"
  val jsr305V = "3.0.0"
  val kittensV = "1.0.0-RC0"
  val liquibaseSlf4jV = "2.0.0"
  val liquibaseV = "3.5.1"
  val logbackV = "1.2.3"
  val metrics3StatsdV = "4.2.0"
  val metricsScalaV = "3.5.6"
  val mockserverNettyV = "3.10.2"
  val mouseV = "0.10-MF"
  val mysqlV = "5.1.42"
  val nettyHandlerV = "4.0.51.Final"
  val pegdownV = "1.6.0"
  val protoGoogleCommonProtosV = "0.1.21"
  val protoGoogleIamV1V = "0.1.21"
  val ravenLogbackV = "8.0.3"
  val refinedV = "0.8.3"
  val scalaGraphV = "1.12.0"
  val scalaLoggingV = "3.6.0"
  val scalacheckV = "1.13.4"
  val scalacticV = "3.0.1"
  val scalameterV = "0.8.2"
  val scalatestV = "3.0.2"
  val scoptV = "3.6.0"
  val shapelessV = "2.3.2"
  val slf4jV = "1.7.24"
  val slickV = "3.2.0"
  val snakeyamlV = "1.17"
  val specs2MockV = "3.8.9" // 3.9.X doesn't enjoy the spark backend or refined
  val sttpV = "0.0.16"
  val swaggerParserV = "1.0.22"
  val swaggerUiV = "3.2.2"
  val typesafeConfigV = "1.3.1"

  /*
  If you see warnings from SBT about evictions, insert a specific dependency version into this list.
   */
  val cromwellDependencyOverrides = List(
    "com.google.api.grpc" % "proto-google-common-protos" % protoGoogleCommonProtosV,
    "com.google.api.grpc" % "proto-google-iam-v1" % protoGoogleIamV1V,
    "com.google.auth" % "google-auth-library-credentials" % googleCredentialsV,
    "com.google.auth" % "google-auth-library-oauth2-http" % googleOauth2V,
    "com.google.code.findbugs" % "jsr305" % jsr305V,
    "com.google.guava" % "guava" % guavaV,
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-stream" % akkaV,
    "io.netty" % "netty-handler" % nettyHandlerV,
    "org.typelevel" %% "cats-core" % catsV,
    "org.typelevel" %% "cats-kernel" % catsV
  )

  // Internal collections of dependencies

  private val fs2Test = "co.fs2" %% "fs2-io" % fs2V % Test

  private val catsDependencies = List(
    "org.typelevel" %% "cats-core" % catsV,
    "com.github.benhutchison" %% "mouse" % mouseV,
    "org.typelevel" %% "kittens" % kittensV
  )

  private val baseDependencies = List(
    "com.iheart" %% "ficus" % ficusV,
    "org.specs2" %% "specs2-mock" % specs2MockV % Test
  ) ++ catsDependencies :+ fs2Test

  private val slf4jBindingDependencies = List(
    // http://logback.qos.ch/dependencies.html
    "ch.qos.logback" % "logback-classic" % logbackV,
    "ch.qos.logback" % "logback-access" % logbackV,
    "com.getsentry.raven" % "raven-logback" % ravenLogbackV,
    "org.codehaus.janino" % "janino" % janinoV
  )

  private val slickDependencies = List(
    "com.typesafe.slick" %% "slick" % slickV,
    "com.typesafe.slick" %% "slick-hikaricp" % slickV
  )

  private val liquibaseDependencies = List(
    "org.liquibase" % "liquibase-core" % liquibaseV,
    // This is to stop liquibase from being so noisy by default
    // See: http://stackoverflow.com/questions/20880783/how-to-get-liquibase-to-log-using-slf4j
    "com.mattbertolini" % "liquibase-slf4j" % liquibaseSlf4jV
  )

  private val akkaHttpDependencies = List(
    "com.typesafe.akka" %% "akka-http" % akkaHttpV,
    "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpV % Test
  )

  private val swaggerUiDependencies = List(
    "org.webjars" % "swagger-ui" % swaggerUiV
  )

  private val googleApiClientDependencies = List(
    // Used by swagger, but only in tests.  This overrides an older 2.1.3 version of jackson-core brought in by
    // these Google dependencies, but which isn't properly evicted by IntelliJ's sbt integration.
    "com.fasterxml.jackson.core" % "jackson-core" % jacksonV,
    // The exclusions prevent guava from colliding at assembly time.
    "com.google.guava" % "guava" % guavaV,
    "com.google.api-client" % "google-api-client-java6" % googleClientApiV
      exclude("com.google.guava", "guava-jdk5"),
    "com.google.api-client" % "google-api-client-jackson2" % googleClientApiV
      exclude("com.google.guava", "guava-jdk5")
  )

  private val googleCloudDependencies = List(
    "io.grpc" % "grpc-core" % grpcV,
    "com.google.guava" % "guava" % guavaV,
    "com.google.apis" % "google-api-services-genomics" % googleGenomicsServicesApiV
      exclude("com.google.guava", "guava-jdk5"),
    "com.google.cloud" % "google-cloud-nio" % googleCloudNioV
      exclude("com.google.api.grpc", "grpc-google-common-protos")
      exclude("com.google.cloud.datastore", "datastore-v1-protos")
      exclude("org.apache.httpcomponents", "httpclient"),
    "com.google.cloud" % "google-cloud-compute" % googleCloudComputeV,
    "org.apache.httpcomponents" % "httpclient" % apacheHttpClientV
  )

  private val dbmsDependencies = List(
    "org.hsqldb" % "hsqldb" % hsqldbV,
    /*
    When going to 6.0.x, will need to change the jdbc driver to com.mysql.cj.jdbc.Driver
    - https://dev.mysql.com/doc/connector-j/6.0/en/connector-j-api-changes.html

    The url may also need the parameters:
    - serverTimezone=UTC via http://stackoverflow.com/a/36793896/3320205
    - nullNamePatternMatchesAll=true via https://liquibase.jira.com/browse/CORE-2723
     */
    "mysql" % "mysql-connector-java" % mysqlV
  )

  private val refinedTypeDependenciesList = List(
    "eu.timepit" %% "refined" % refinedV
  )

  private val circeYamlDependency = "io.circe" %% "circe-yaml" % circeYamlV
  private val circeDependencies = List(
    "core",
    "parser",
    "generic",
    "generic-extras",
    "shapes",
    "refined",
    "literal"
  ).map(m => "io.circe" %% s"circe-$m" % circeV) :+ circeYamlDependency

  // Sub-project dependencies, added in addition to any dependencies inherited from .dependsOn().

  val cloudSupportDependencies = googleApiClientDependencies ++ googleCloudDependencies ++ List(
    "com.github.pathikrit" %% "better-files" % betterFilesV
  )

  val databaseSqlDependencies = baseDependencies ++ slickDependencies ++ dbmsDependencies ++ refinedTypeDependenciesList

  val statsDDependencies = List(
    "nl.grons" %% "metrics-scala" % metricsScalaV,
    "com.readytalk" % "metrics3-statsd" % metrics3StatsdV
  )

  val commonDependencies = List(
    "com.typesafe" % "config" % typesafeConfigV,
    "org.slf4j" % "slf4j-api" % slf4jV,
    "com.iheart" %% "ficus" % ficusV
  ) ++ catsDependencies ++ slf4jBindingDependencies

  val womDependencies = List(
    "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingV,
    "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpV,
    "org.apache.commons" % "commons-text" % commonsTextV,
    "commons-codec" % "commons-codec" % commonsCodecV
  ) ++ commonDependencies

  val wdlDependencies = List(
    "commons-io" % "commons-io" % commonsIoV,
    "com.github.pathikrit" %% "better-files" % betterFilesV,
    "org.scala-graph" %% "graph-core" % scalaGraphV,
    "com.chuusai" %% "shapeless" % shapelessV,
    "com.softwaremill.sttp" %% "core" % sttpV,
    "com.softwaremill.sttp" %% "async-http-client-backend-cats" % sttpV,
    "org.mock-server" % "mockserver-netty" % mockserverNettyV % Test
  )

  val cwlDependencies = List(
    "com.github.pathikrit" %% "better-files" % betterFilesV,
    "com.lihaoyi" %% "ammonite-ops" % ammoniteOpsV,
    "org.typelevel" %% "cats-effect" % catsEffectV,
    "org.scalactic" %% "scalactic" % scalacticV,
    "org.scalacheck" %% "scalacheck" % scalacheckV % Test
  ) ++ circeDependencies ++ womDependencies ++ refinedTypeDependenciesList

  val womtoolDependencies = catsDependencies

  val centaurCwlRunnerDependencies = List(
    "com.github.scopt" %% "scopt" % scoptV
  )

  val coreDependencies = List(
    "com.typesafe" % "config" % typesafeConfigV,
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-slf4j" % akkaV,
    "com.typesafe.akka" %% "akka-testkit" % akkaV % Test,
    "com.google.auth" % "google-auth-library-oauth2-http" % googleOauth2V,
    "com.typesafe.akka" %% "akka-stream-testkit" % akkaV,
    "com.chuusai" %% "shapeless" % shapelessV,
    "com.github.scopt" %% "scopt" % scoptV,
    "com.github.pathikrit" %% "better-files" % betterFilesV
  ) ++ baseDependencies ++ googleApiClientDependencies ++ statsDDependencies ++
    // TODO: We're not using the "F" in slf4j. Core only supports logback, specifically the WorkflowLogger.
    slf4jBindingDependencies

  val databaseMigrationDependencies = liquibaseDependencies ++ dbmsDependencies

  val cromwellApiClientDependencies = List(
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpV,
    "com.typesafe.akka" %% "akka-stream" % akkaV,
    "com.github.pathikrit" %% "better-files" % betterFilesV
  ) ++ akkaHttpDependencies

  val centaurDependencies = List(
    "com.github.kxbmap" %% "configs" % configsV
  ) ++ circeDependencies ++ googleCloudDependencies

  val engineDependencies = List(
    "commons-codec" % "commons-codec" % commonsCodecV,
    "commons-io" % "commons-io" % commonsIoV,
    "com.storm-enroute" %% "scalameter" % scalameterV
      exclude("com.fasterxml.jackson.core", "jackson-databind")
      exclude("com.fasterxml.jackson.module", "jackson-module-scala")
      exclude("org.scala-tools.testing", "test-interface"),
    "com.fasterxml.jackson.core" % "jackson-databind" % jacksonV,
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % jacksonV,
    "io.swagger" % "swagger-parser" % swaggerParserV % Test,
    "org.yaml" % "snakeyaml" % snakeyamlV % Test
  ) ++ swaggerUiDependencies ++ akkaHttpDependencies

  val rootDependencies = slf4jBindingDependencies

  val tesBackendDependencies = akkaHttpDependencies
  val sparkBackendDependencies = akkaHttpDependencies

  val testDependencies = List(
    "org.scalatest" %% "scalatest" % scalatestV,
    "org.pegdown" % "pegdown" % pegdownV
  )
}