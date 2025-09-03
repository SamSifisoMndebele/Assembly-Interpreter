plugins {
    kotlin("jvm") version "2.2.0"
}

group = "za.co.studentintellect.asm"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(kotlin("test"))
    implementation("org.jetbrains.kotlin:kotlin-reflect")
}

tasks.test {
    useJUnitPlatform()
}
kotlin {
    jvmToolchain(22)
}