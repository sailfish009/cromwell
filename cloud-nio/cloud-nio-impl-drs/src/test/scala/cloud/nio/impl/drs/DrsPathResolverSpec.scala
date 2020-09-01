package cloud.nio.impl.drs

import java.time.OffsetDateTime

import cloud.nio.impl.drs.MarthaResponseSupport.{convertMarthaResponseV2ToV3, _}
import io.circe.parser._
import io.circe.{Json, JsonObject}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DrsPathResolverSpec extends AnyFlatSpecLike with Matchers {
  private val mockGSA = SADataObject(data = Json.fromJsonObject(JsonObject("key"-> Json.fromString("value"))))
  private val crcHashValue = "8a366443"
  private val md5HashValue = "336ea55913bc261b72875bd259753046"
  private val shaHashValue = "f76877f8e86ec3932fd2ae04239fbabb8c90199dab0019ae55fa42b31c314c44"
  private val fullMarthaV2Response: String =
    s"""{
       |  "dos": {
       |    "data_object": {
       |      "size": 34905345,
       |      "updated": "2020-04-27T15:56:09.696Z",
       |      "urls": [
       |        {
       |          "url": "s3://my-s3-bucket/file-name"
       |        },
       |        {
       |          "url": "gs://my-gs-bucket/file-name"
       |        }
       |      ],
       |      "gsUri": "gs://my-gs-bucket/file-name",
       |      "checksums": [
       |        {
       |           "type": "md5",
       |           "checksum": "$md5HashValue"
       |        },
       |        {
       |           "type": "crc32c",
       |           "checksum": "$crcHashValue"
       |        }
       |      ]
       |    }
       |  },
       |  "googleServiceAccount": {
       |    "data": {
       |      "key": "value"
       |    }
       |  }
       |}
       |""".stripMargin

  private val fullMarthaV2ResponseNoTz: String =
    s"""{
       |  "dos": {
       |    "data_object": {
       |      "size": 34905345,
       |      "updated": "2020-04-27T15:56:09.696",
       |      "urls": [
       |        {
       |          "url": "s3://my-s3-bucket/file-name"
       |        },
       |        {
       |          "url": "gs://my-gs-bucket/file-name"
       |        }
       |      ],
       |      "gsUri": "gs://my-gs-bucket/file-name",
       |      "checksums": [
       |        {
       |           "type": "md5",
       |           "checksum": "$md5HashValue"
       |        },
       |        {
       |           "type": "crc32c",
       |           "checksum": "$crcHashValue"
       |        }
       |      ]
       |    }
       |  },
       |  "googleServiceAccount": {
       |    "data": {
       |      "key": "value"
       |    }
       |  }
       |}
       |""".stripMargin

  private val fullMarthaV3Response: String =
    s"""|{
        |  "size": 34905345,
        |  "timeUpdated": "2020-04-27T15:56:09.696Z",
        |  "bucket": "my-gs-bucket",
        |  "name": "file-name",
        |  "gsUri": "gs://my-gs-bucket/file-name",
        |  "googleServiceAccount": {
        |    "data": {
        |      "key": "value"
        |    }
        |  },
        |  "hashes": {
        |     "md5": "$md5HashValue",
        |     "crc32c": "$crcHashValue"
        |  }
        |}
        |""".stripMargin

  private val expectedMarthaResponse: MarthaResponse = MarthaResponse(
    size = Option(34905345),
    timeUpdated = Option(OffsetDateTime.parse("2020-04-27T15:56:09.696Z")),
    bucket = Option("my-gs-bucket"),
    name = Option("file-name"),
    gsUri = Option("gs://my-gs-bucket/file-name"),
    googleServiceAccount = Option(mockGSA),
    hashes = Option(Map("md5" -> md5HashValue, "crc32c" -> crcHashValue))
  )

  private val alfredHashValue = "xrd"
  private val completeHashesMap = Map(
    "betty" -> "abc123",
    "charles" -> "456",
    "alfred" -> alfredHashValue,
    "sha256" -> shaHashValue,
    "crc32c" -> crcHashValue,
    "md5" -> md5HashValue,
  )

  private val missingCRCHashesMap = Map(
    "alfred" -> alfredHashValue,
    "sha256" -> shaHashValue,
    "betty" -> "abc123",
    "md5" -> md5HashValue,
    "charles" -> "456",
  )

  private val onlySHAHashesMap = Map(
    "betty" -> "abc123",
    "charles" -> "456",
    "alfred" -> alfredHashValue,
    "sha256" -> shaHashValue,
  )

  private val noPreferredHashesMap = Map(
    "alfred" -> alfredHashValue,
    "betty" -> "abc123",
    "charles" -> "456",
  )

  behavior of "fileHash()"

  it should "return crc32c hash from `hashes` in Martha response when there is a crc32c" in {
    DrsCloudNioRegularFileAttributes.getPreferredHash(completeHashesMap) shouldBe Option(crcHashValue)
  }

  it should "return md5 hash from `hashes` in Martha response when there is no crc32c" in {
    DrsCloudNioRegularFileAttributes.getPreferredHash(missingCRCHashesMap) shouldBe Option(md5HashValue)
  }

  it should "return sha256 hash from `hashes` in Martha response when there is only a sha256" in {
    DrsCloudNioRegularFileAttributes.getPreferredHash(onlySHAHashesMap) shouldBe Option(shaHashValue)
  }

  it should "return first (alphabetized by type) hash from `hashes` in Martha response when there are no preferred hash types" in {
    DrsCloudNioRegularFileAttributes.getPreferredHash(noPreferredHashesMap) shouldBe Option(alfredHashValue)
  }

  behavior of "convertMarthaResponseV2ToV3()"

  it should "convert a full martha_v2 response to a the standard Martha response" in {
    decode[MarthaV2Response](fullMarthaV2Response).map(convertMarthaResponseV2ToV3) shouldBe
      Right(expectedMarthaResponse)
  }

  it should "convert a full martha_v2 response to a the standard Martha response even if there is no timezone in `updated` field" in {
    decode[MarthaV2Response](fullMarthaV2ResponseNoTz).map(convertMarthaResponseV2ToV3) shouldBe
      Right(expectedMarthaResponse)
  }

  it should "convert a full martha_v3 response to a the standard Martha response" in {
    decode[MarthaResponse](fullMarthaV3Response) shouldBe Right(expectedMarthaResponse)
  }
}
